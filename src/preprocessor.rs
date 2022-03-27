use std::collections::{HashMap, VecDeque};

use crate::ast;

struct Input {
    source: VecDeque<ast::Token>,
    pos: usize,
}

enum Macro {
    ObjectLike {
        replacement_list: Vec<ast::Token>,
    },
    FunctionLike {
        arguments: Vec<String>,
        replacement_list: Vec<ast::Token>,
    },
}

struct Defines {
    macros: HashMap<String, Macro>,
}

struct State {
    input: Input,
    output: Vec<ast::Token>,
    defines: Defines,
}

impl Default for Defines {
    fn default() -> Self {
        Self {
            macros: Default::default(),
        }
    }
}

impl Defines {
    fn add_macro(
        &mut self,
        name: String,
        replacement_list: Vec<ast::Token>,
    ) -> std::io::Result<()> {
        use std::collections::hash_map::Entry::*;
        match self.macros.entry(name) {
            Occupied(_) => todo!("handle macro redefinition"),
            Vacant(v) => {
                v.insert(Macro::ObjectLike {
                    replacement_list: replacement_list,
                });
                Ok(())
            }
        }
    }

    fn is_object_like_macro(&self, token: &ast::Token) -> bool {
        match token.tt {
            ast::TokenType::Identifier(ref name) => self.macros.contains_key(name),
            _ => false,
        }
    }

    fn get_object_like_macro(&self, token: &ast::Token) -> Option<(String, &Macro)> {
        let ident_name = match token.tt {
            ast::TokenType::Identifier(ref name) => name,
            _ => {
                // Token isn't an identifier
                return None;
            }
        };

        let mcro = self.macros.get(ident_name)?;

        if let Some(hideset) = &token.hideset {
            if hideset.contains(ident_name) {
                // Macro exists but is hiden for this token
                return None;
            } 
        }

        Some((ident_name.to_owned(), mcro))
    }
}

impl Input {
    fn next(&mut self) -> Option<ast::Token> {
        self.source.pop_front()
    }

    fn peek(&self) -> Option<ast::Token> {
        self.source.front().cloned()
    }

    fn peek_type(&self) -> Option<ast::TokenType> {
        self.peek().map(|t| t.tt)
    }

    fn unget(&mut self, items: Vec<ast::Token>) {
       for item in items.into_iter().rev() {
           self.source.push_front(item);
       }
    }
}

impl From<Vec<ast::Token>> for Input {
    fn from(tokens: Vec<ast::Token>) -> Self {
        Self {
            source: tokens.into(),
            pos: 0,
        }
    }
}

impl From<Vec<ast::Token>> for State {
    fn from(tokens: Vec<ast::Token>) -> Self {
        Self {
            input: tokens.into(),
            output: Default::default(),
            defines: Default::default(),
        }
    }
}

impl Into<Vec<ast::Token>> for State {
    fn into(self) -> Vec<ast::Token> {
        self.output
    }
}

pub fn preprocess(
    tokens: Vec<ast::Token>,
    _lex_file: fn(&str) -> std::io::Result<Vec<ast::Token>>,
) -> std::io::Result<Vec<ast::Token>> {
    let mut state = State::from(tokens);

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
impl State {
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
                    self.parse_control_line()?;
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
     *  control-line:
     *      : # include pp-tokens new-line
     *      | # define identifier replacement-list new-line
     *      | # define identifier lparen identifier-list[opt] ) replacement-list new-line
     *      | # define identifier lparen ... ) replacement-list new-line
     *      | # define identifier lparen identifier-list , ... ) replacement-list new-line
     *      | # undef identifier new-line
     *      | # line pp-tokens new-line
     *      | # error pp-tokens[opt] new-line
     *      | # pragma pp-tokens[opt] new-line
     *      | # new-line
     *      ;
     */
    fn parse_control_line(&mut self) -> std::io::Result<()> {
        if self.is_new_line() {
            return Ok(());
        }

        let ident = match self.input.peek_type() {
            Some(ast::TokenType::Identifier(ident)) => ident.to_lowercase(),
            _ => unimplemented!("unexpected non identifier following #"),
        };

        self.input.next();

        match ident.as_str() {
            "include" => unimplemented!(),
            "define" => {
                let macro_name = match self.input.next() {
                    Some(ast::Token {
                        tt: ast::TokenType::Identifier(name),
                        ..
                    }) => name,
                    _ => unimplemented!("macro names must be identifiers"),
                };

                match self.input.peek_type() {
                    /*  # define identifier lparen identifier-list[opt] ) replacement-list new-line
                     *  # define identifier lparen ... ) replacement-list new-line
                     *  # define identifier lparen identifier-list , ... ) replacement-list new-line
                     */
                    Some(ast::TokenType::LParen) => {
                        unimplemented!("function like macro")
                    }
                    /* # define identifier replacement-list new-line
                     */
                    _ => {
                        let replacement_list = self.parse_replacement_list()?;
                        self.defines.add_macro(macro_name, replacement_list)?;

                        Ok(())
                    }
                }
            }
            "undef" => unimplemented!(),
            "line" => unimplemented!(),
            "error" => unimplemented!(),
            "pragma" => unimplemented!(),
            _ => unimplemented!("unknown identifier following #"),
        }
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

    /**
     *      replacement-list:
     *         : pp-tokens[opt]
     *         ;
     *
     *      pp-tokens:
     *         : preprocessing-token
     *         | pp-tokens preprocessing-token
     *         ;
     */
    fn parse_replacement_list(&mut self) -> std::io::Result<Vec<ast::Token>> {
        let mut tokens = Vec::new();
        while self.is_preprocessing_token() {
            let token = self.input.next().unwrap();
            tokens.push(token);

            if self.is_new_line() {
                break;
            }
        }

        Ok(tokens)
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

    /**
     *
     *  # if ..  
     *  # ifdef ..  
     *  # ifndef ..
     */
    fn is_if_section(&self) -> bool {
        if self.is_new_line() {
            return false;
        }

        match self.input.peek_type() {
            Some(ast::TokenType::Identifier(ident)) => {
                let ident = ident.to_lowercase();
                ident == "if" || ident == "ifdef" || ident == "ifndef"
            }
            _ => false,
        }
    }

    /**
     *  # include ..
     *  # define ..
     *  # undef ..
     *  # line ..
     *  # error ..
     *  # pragma ..
     *  # new-line
     */
    fn is_control_section(&self) -> bool {
        if self.is_new_line() {
            return true;
        }

        match self.input.peek_type() {
            Some(ast::TokenType::Identifier(ident)) => {
                let ident = ident.to_lowercase();
                ident == "include"
                    || ident == "define"
                    || ident == "undef"
                    || ident == "line"
                    || ident == "error"
                    || ident == "pragma"
            }
            _ => false,
        }
    }
}

/**
 * contains the non parsing functions
 */
impl State {
    /**
     * expand and emit
     */
    fn emit(&mut self, token: ast::Token) {
        if let Some((name, mcro)) = self.defines.get_object_like_macro(&token) {
            match mcro {
                Macro::ObjectLike { replacement_list } => {
                    let replacement_list = replacement_list.iter().map(|t|{
                        let mut new_token = t.clone();
                        new_token.token_start = token.token_start;
                        new_token.token_end = token.token_end;
                        new_token.is_bol = false; // TODO: consider options
                        new_token.is_wsep = true; // TODO: consider options
                        new_token.pp_hide(name.clone());
                        new_token
                    });

                    self.input.unget(replacement_list.collect());
                },
                Macro::FunctionLike { arguments, replacement_list } => todo!(),
            }
        } else {
            self.output.push(token);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::TokenType::*;
    use crate::ast::{Token, TokenType};
    use crate::lexer::lex_string;
    use crate::preprocessor::preprocess;

    fn noop_load(_: &str) -> std::io::Result<Vec<Token>> {
        panic!("no loads expected");
    }

    // Token list equality
    macro_rules! token_eq {
     ($lhs:expr, $($tok:expr)*) => {
         assert_eq!($lhs, vec![$($tok, )*]);
     };
    }

    macro_rules! tok {
        (id $ident:ident) => {
            TokenType::Identifier(stringify!($ident).to_owned())
        };
    }

    fn test_preprocessor(input: &str) -> Vec<TokenType> {
        let tokens = lex_string(String::from(input), "-").unwrap();
        let tokens = preprocess(tokens, noop_load).unwrap();

        tokens.into_iter().map(|t| t.tt).collect()
    }

    #[test]
    fn test_pp() {
        let tokens = test_preprocessor("(");
        token_eq!(tokens, LParen);
    }

    #[test]
    fn test_pp2() {
        let tokens = test_preprocessor("{{}}");
        token_eq!(tokens, LBrace LBrace RBrace RBrace);
    }

    #[test]
    fn test_define() {
        let tokens = test_preprocessor("#define A B\nA A");

        token_eq!(tokens, tok!(id B) tok!(id B));
    }
}
