use std::collections::{HashMap, VecDeque};

use crate::ast;

struct Input {
    source: VecDeque<ast::Token>,
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
    lex_file_fn: fn(&str) -> std::io::Result<Vec<ast::Token>>,
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

    fn add_function_macro(
        &mut self,
        name: String,
        macro_arguments: Vec<String>,
        replacement_list: Vec<ast::Token>,
    ) -> std::io::Result<()> {
        use std::collections::hash_map::Entry::*;
        match self.macros.entry(name) {
            Occupied(_) => todo!("handle macro redefinition"),
            Vacant(v) => {
                v.insert(Macro::FunctionLike {
                    replacement_list: replacement_list,
                    arguments: macro_arguments,
                });
                Ok(())
            }
        }
    }

    fn get_macro(&self, token: &ast::Token) -> Option<(String, &Macro)> {
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

    fn remove_macro(&mut self, macro_name: String) -> std::io::Result<()> {
        use std::collections::hash_map::Entry::*;
        match self.macros.entry(macro_name) {
            Occupied(entry) => entry.remove(),
            Vacant(_) => todo!("check if you can remove a non existent macro"),
        };

        Ok(())
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
    lex_file: fn(&str) -> std::io::Result<Vec<ast::Token>>,
) -> std::io::Result<Vec<ast::Token>> {
    let mut state = State::new(tokens, lex_file);

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
            "include" => {
                let include_path = match self.input.next() {
                    Some(ast::Token {
                        tt: ast::TokenType::StringLiteral(include_name),
                        ..
                    }) => include_name,
                    Some(ast::Token {
                        tt: ast::TokenType::LessThan,
                        ..
                    }) => match self.input.next() {
                        Some(ast::Token {
                            tt: ast::TokenType::Identifier(include_name),
                            ..
                        }) => include_name,
                        _ => {
                            unimplemented!(
                                "unexpected token after #include '<'  expected 'path' '>'"
                            );
                        }
                    },
                    _ => {
                        unimplemented!(
                            "unexpected token after #include expected '<' 'path' '>' or '\"path\"'"
                        );
                    }
                };

                self.include_file(&include_path)?;

                Ok(())
            }
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
                        self.input.next();
                        let macro_arguments =
                            if self.input.peek_type() == Some(ast::TokenType::RParen) {
                                self.input.next();
                                Vec::new()
                            } else {
                                let identifier_list = self.parse_identifier_list()?;
                                assert_eq!(self.input.peek_type(), Some(ast::TokenType::RParen));
                                self.input.next();
                                identifier_list
                            };

                        let replacement_list = self.parse_replacement_list()?;

                        self.defines.add_function_macro(
                            macro_name,
                            macro_arguments,
                            replacement_list,
                        )?;

                        Ok(())
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
            "undef" => {
                let macro_name = match self.input.next() {
                    Some(ast::Token {
                        tt: ast::TokenType::Identifier(name),
                        ..
                    }) => name,
                    _ => unimplemented!("macro names must be identifiers"),
                };

                if !self.is_new_line() {
                    unimplemented!("undef directive has more tokens, gcc accepts this");
                }

                self.defines.remove_macro(macro_name)?;

                Ok(())
            }
            "line" => unimplemented!(),
            "error" => {
                todo!("# error encountered while preprocessing, enhance errors");
            }
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
            self.emit(token)?;

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

    /**
     * identifier-list:
     *    : identifier
     *    : identifier-list , identifier
     *    ;
     */
    fn parse_identifier_list(&mut self) -> std::io::Result<Vec<String>> {
        let mut identifier_list = Vec::new();

        match self.input.peek_type() {
            Some(ast::TokenType::Identifier(identifier)) => {
                self.input.next().unwrap();
                identifier_list.push(identifier);
            }
            _ => {
                return Ok(identifier_list);
            }
        }

        loop {
            match self.input.peek_type() {
                Some(ast::TokenType::Comma) => {
                    self.input.next().unwrap();
                }
                _ => return Ok(identifier_list),
            };

            match self.input.peek_type() {
                Some(ast::TokenType::Identifier(identifier)) => {
                    self.input.next().unwrap();
                    identifier_list.push(identifier);
                }
                _ => {
                    unimplemented!("Unexpected token type after comma in identifier list");
                }
            }
        }
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
    fn new(
        tokens: Vec<ast::Token>,
        lex_file: fn(&str) -> std::io::Result<Vec<ast::Token>>,
    ) -> Self {
        Self {
            input: tokens.into(),
            output: Default::default(),
            defines: Default::default(),
            lex_file_fn: lex_file,
        }
    }
    /**
     * expand and emit
     */
    fn emit(&mut self, token: ast::Token) -> std::io::Result<()> {
        let instance_token = |new_token: &ast::Token, source_token: &ast::Token, name: &String| {
            let mut new_token = new_token.clone();
            new_token.token_start = source_token.token_start;
            new_token.token_end = source_token.token_end;
            new_token.hideset = source_token.hideset.clone();
            new_token.pp_hide(name.clone());
            new_token
        };

        let instance_token_list =
            |new_token_list: &Vec<ast::Token>, source_token: &ast::Token, name: &String| {
                let mut replacement_list: Vec<_> = new_token_list
                    .iter()
                    .map(|new_token| instance_token(new_token, &source_token, &name))
                    .collect();

                if let Some(first) = replacement_list.iter_mut().nth(0) {
                    first.is_bol = token.is_bol;
                    first.is_wsep = token.is_wsep;
                }

                replacement_list
            };

        if let Some((name, mcro)) = self.defines.get_macro(&token) {
            match mcro {
                Macro::ObjectLike { replacement_list } => {
                    let replacement_list = instance_token_list(replacement_list, &token, &name);
                    self.input.unget(replacement_list);
                }
                Macro::FunctionLike {
                    arguments,
                    replacement_list,
                } => {
                    // now we have to parse the argument list for the macro
                    let macro_arguments: Vec<Vec<ast::Token>> =
                        self.input.parse_macro_argument_list()?;

                    assert_eq!(
                        macro_arguments.len(),
                        arguments.len(),
                        "function like macro called with incorrect number of arguments"
                    );

                    // create map from token to argument index.
                    let argument_index_map: HashMap<String, usize> =
                        arguments.iter().cloned().zip(0..).collect();

                    let mut list = Vec::new();

                    // unget tokens from the replacement list, stopping when any token matches an argument in the argument map and ungetting that instead.
                    for new_token in replacement_list {
                        if let ast::TokenType::Identifier(ident) = &new_token.tt {
                            if let Some(index) = argument_index_map.get(ident).cloned() {
                                let arg = macro_arguments.get(index).unwrap();
                                let replacement_list = instance_token_list(arg, &token, &name);
                                for token in replacement_list {
                                    list.push(token);
                                }
                                continue;
                            }
                        }

                        let token = instance_token(new_token, &token, &name);
                        list.push(token);
                    }

                    self.input.unget(list);
                }
            }
        } else {
            self.output.push(token);
        }

        Ok(())
    }

    fn include_file(&mut self, include_path: &str) -> std::io::Result<()> {
        let result = (self.lex_file_fn)(include_path)?;

        self.input.unget(result);

        Ok(())
    }
}

impl Input {
    /**
     * a macro argument list starts and ends with a ( and ) token and contains a , seperated sequence of tokens
     *
     * we skip all intervening matched pairs of ( and )
     *
     * (a,b,c) => [a, b, c]
     * (a a, b) => [a a, b]
     * (a(c), d) => [a(c), d]
     * (a(c,f), d) => [a(c,f), d]
     */
    fn parse_macro_argument_list(&mut self) -> std::io::Result<Vec<Vec<ast::Token>>> {
        assert_eq!(self.peek_type(), Some(ast::TokenType::LParen));
        self.next().unwrap();

        let mut argument_list: Vec<Vec<ast::Token>> = Vec::new();
        argument_list.push(Default::default());

        loop {
            match self.peek_type() {
                Some(ast::TokenType::Comma) => {
                    self.next();
                    argument_list.push(Default::default());
                }
                Some(ast::TokenType::LParen) => {
                    let mut tokens = self.parse_matching_pair()?;                    
                    argument_list.last_mut().unwrap().append(&mut tokens);
                },
                Some(ast::TokenType::RParen) => {
                    self.next();
                    break;
                }
                Some(_) => {
                    let token = self.next().unwrap();
                    argument_list.last_mut().unwrap().push(token);
                }
                None => unimplemented!(),
            }
        }

        Ok(argument_list)
    }

    fn parse_matching_pair(&mut self) -> std::io::Result<Vec<ast::Token>> {
        let mut tokens = Vec::new();

        macro_rules! append_token {
            () => {
                tokens.push(self.next().unwrap());
            }
        }


        assert_eq!(self.peek_type(), Some(ast::TokenType::LParen));
        append_token!();

        loop {
            match self.peek_type() {
                Some(ast::TokenType::LParen) => {
                    let mut nested_tokens = self.parse_matching_pair()?;
                    tokens.append(&mut nested_tokens);
                }
                Some(ast::TokenType::RParen) => {
                    break;
                }
                Some(_) => {
                    append_token!();
                }
                None => {
                    unimplemented!("unexpected EOF while parsing function like macro arguments");
                }
            }
        }

        assert_eq!(self.peek_type(), Some(ast::TokenType::RParen));
        append_token!();

        Ok(tokens)
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

    #[test]
    fn test_define_circle() {
        let tokens = test_preprocessor("#define A B\n#define B A\nA B");

        token_eq!(tokens, tok!(id A) tok!(id B));
    }
}
