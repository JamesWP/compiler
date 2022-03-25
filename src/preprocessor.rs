use crate::ast;

struct PreprocessorInput {
    source: Vec<ast::Token>,
    pos: usize,
}

struct PreprocessorState {
    input: PreprocessorInput,
    output: Vec<ast::Token>,
}

impl PreprocessorState {
    fn emit(&mut self, token: ast::Token) {
        self.output.push(token);
    }
}

impl PreprocessorInput {
    fn next(&mut self) -> Option<ast::Token> {
        if self.pos < self.source.len() {
            self.pos += 1;
            Some(self.source[self.pos - 1].clone())
        } else {
            None
        }
    }

    fn peek(&self) -> Option<ast::Token> {
        if self.pos < self.source.len() {
            Some(self.source[self.pos].clone())
        } else {
            None
        }
    }

    fn peek_type(&self) -> Option<ast::TokenType> {
        self.peek().map(|t| t.tt)
    }
}

impl From<Vec<ast::Token>> for PreprocessorInput {
    fn from(tokens: Vec<ast::Token>) -> Self {
        Self {
            source: tokens,
            pos: 0,
        }
    }
}

impl From<Vec<ast::Token>> for PreprocessorState {
    fn from(tokens: Vec<ast::Token>) -> Self {
        Self {
            input: tokens.into(),
            output: Default::default(),
        }
    }
}

impl Into<Vec<ast::Token>> for PreprocessorState {
    fn into(self) -> Vec<ast::Token> {
        self.output
    }
}

pub fn preprocess(
    tokens: Vec<ast::Token>,
    _lex_file: fn(&str) -> std::io::Result<Vec<ast::Token>>,
) -> std::io::Result<Vec<ast::Token>> {
    let mut state = PreprocessorState::from(tokens);

    state.parse_preprocessing_file()?;

    Ok(state.into())
}

/**
 * 2007 C Draft: http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf
 *
 *      preprocessing-file:
 *         : group[opt]
 *         ;
 *
 *      group:
 *         : group-part
 *         | group group-part
 *         ;
 *
 *      group-part:
 *         : if-section
 *         | control-line
 *         | text-line
 *         | # non-directive
 *         ;
 *
 *      if-section:
 *         : if-group elif-groups[opt] else-group[opt] endif-line
 *         ;
 *
 *      if-group:
 *         : # if constant-expression new-line group[opt]
 *         | # ifdef identifier new-line group[opt]
 *         | # ifndef identifier new-line group[opt]
 *         ;
 *
 *      elif-groups:
 *         : elif-group
 *         | elif-groups elif-group
 *         ;
 *
 *      elif-group:
 *         : # elif constant-expression new-line group[opt]
 *         ;
 *
 *      else-group:
 *         : # else new-line group[opt]
 *         ;
 *
 *      endif-line:
 *         : # endif new-line
 *         ;
 *          
 *      control-line:
 *         : # include pp-tokens new-line
 *         | # define identifier replacement-list new-line
 *         | # define identifier lparen identifier-list[opt] ) replacement-list new-line
 *         | # define identifier lparen ... ) replacement-list new-line
 *         | # define identifier lparen identifier-list , ... ) replacement-list new-line
 *         | # undef identifier new-line
 *         | # line pp-tokens new-line
 *         | # error pp-tokens[opt] new-line
 *         | # pragma pp-tokens[opt] new-line
 *         | # new-line
 *         ;
 *          
 *      text-line:
 *         : pp-tokens[opt] new-line
 *         ;
 *
 *      non-directive:
 *         : pp-tokens new-line
 *         ;
 *
 *      lparen:
 *         : "a ( character not immediately preceded by white-space"
 *         ;
 *
 *      replacement-list:
 *         : pp-tokens[opt]
 *         ;
 *
 *      pp-tokens:
 *         : preprocessing-token
 *         | pp-tokens preprocessing-token
 *         ;
 *
 *      new-line:
 *         : "the new-line character"
 *         ;
 *
 */

impl PreprocessorState {
    /**
     *      preprocessing-file:
     *         : group[opt]
     *         ;
     */
    fn parse_preprocessing_file(&mut self) -> std::io::Result<()> {
        if let Some(_) = self.input.peek() {
            self.parse_group()?;
        }

        Ok(())
    }

    /**
     *      group:
     *         : group-part
     *         | group group-part
     *         ;
     */
    fn parse_group(&mut self) -> std::io::Result<()> {
        while let Some(_) = self.input.peek() {
            self.parse_group_part()?;
        }

        Ok(())
    }

    /**
     *      group-part:
     *         : if-section
     *         | control-line
     *         | text-line
     *         | # non-directive
     *         ;
     */
    fn parse_group_part(&mut self) -> std::io::Result<()> {
        match self.input.peek_type() {
            Some(ast::TokenType::Hash) => {
                self.input.next();
                if self.is_if_section() {
                    // must be 'if-section'
                    unimplemented!();
                } else if self.is_control_section() {
                    // must be 'control-section'
                    unimplemented!();
                } else {
                    // must be 'non-directive'
                    unimplemented!();
                }
            }
            Some(_) => {
                self.parse_text_line()?;
            }
            None => panic!("group part needs token"),
        }

        Ok(())
    }

    /**
     *      text-line:
     *         : pp-tokens[opt] new-line
     *         ;
     *
     *      pp-tokens:
     *         : preprocessing-token
     *         | pp-tokens preprocessing-token
     *         ;
     */
    fn parse_text_line(&mut self) -> std::io::Result<()> {
        while self.is_preprocessing_token() {
            let token = self.input.next().unwrap();
            self.emit(token);

            if self.is_new_line() {
                break;
            }
        }

        Ok(())
    }

    fn is_new_line(&self) -> bool {
        if self.input.peek().is_none() {
            true
        } else {
            matches!(self.input.peek(), Some(ast::Token { is_bol: true, .. }))
        }
    }

    fn is_preprocessing_token(&self) -> bool {
        true
    }

    fn is_if_section(&self) -> bool {
        unimplemented!();
    }

    fn is_control_section(&self) -> bool {
        unimplemented!();
    }
}

#[cfg(test)]
mod test {
    use crate::ast::Token;
    use crate::ast::TokenType::*;
    use crate::preprocessor::preprocess;
    use crate::lexer::lex_string;

    macro_rules! tok {
      ($($tok:path)*) => {
        vec![$(Token::from($tok), )*]
      };
    }

    fn noop_load(_: &str) -> std::io::Result<Vec<Token>> {
        panic!("no loads expected");
    }

    #[test]
    fn test_pp() {
        let tokens = preprocess(tok![LParen], noop_load).unwrap();
        assert_eq!(tokens, tok![LParen]);
    }

    #[test]
    fn test_define() {
        let tokens = lex_string("{".to_string(), "-").unwrap();
        let tokens = preprocess(tokens, noop_load).unwrap();
        assert_eq!(tokens, tok![LBrace]);
    }
}
