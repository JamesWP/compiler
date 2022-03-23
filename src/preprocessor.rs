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
            Some(self.source[self.pos-1].clone())
        } else {
            None
        }
    }
}

impl From<Vec<ast::Token>> for PreprocessorInput {
    fn from(tokens: Vec<ast::Token>) -> Self {
        Self { source: tokens, pos: 0 }
    }
}

impl From<Vec<ast::Token>> for PreprocessorState {
    fn from(tokens: Vec<ast::Token>) -> Self {
       Self { input: tokens.into(), output: Default::default() } 
    }
}

impl Into<Vec<ast::Token>> for PreprocessorState {
    fn into(self) -> Vec<ast::Token> {
        self.output
    }
}

pub fn preprocess(tokens: Vec<ast::Token>, _lex_file: fn(&str) -> std::io::Result<Vec<ast::Token>>) -> std::io::Result<Vec<ast::Token>> {
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
 *         | # error pp-tokensopt new-line
 *         | # pragma pp-tokensopt new-line
 *         | # new-line
 *         ;
 *          
 *      text-line:
 *         : pp-tokensopt new-line
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
    fn parse_preprocessing_file(&mut self) -> std::io::Result<()> {
        loop {
            let token = self.input.next();
            if let Some(t) = token {
                self.emit(t);
            } else {
                break;
            }
        }

        Ok(())
    }
}
