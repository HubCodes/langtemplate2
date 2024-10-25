use nom::{
    branch::alt,
    bytes::complete::take_while1,
    character::complete::{char, multispace0},
    combinator::map,
    multi::many0,
    sequence::delimited,
    IResult,
};

#[derive(Debug, PartialEq, Clone)]
pub enum SExpr {
    Int(i64),
    String(String),
    Symbol(String),
    List(Vec<SExpr>),
}

struct Parser;

impl Parser {
    fn parse_atom(input: &str) -> IResult<&str, SExpr> {
        let (input, atom) = alt((
            map(
                take_while1(
                    Self::is_digit_char),
                    |s: &str| SExpr::Int(s.parse::<i64>().unwrap(),
                )
            ),
            map(
                delimited(
                    char('"'), 
                    take_while1(|c| c != '"'),
                    char('"')),
                    |s: &str| SExpr::String(s.to_string(),
                )
            ),
            map(
                take_while1(Self::is_symbol), 
                |s: &str| SExpr::Symbol(s.to_string()),
            ),
        ))(input)?;
        Ok((input, atom))
    }
    
    fn is_digit_char(c: char) -> bool {
        c.is_digit(10)
    }
    
    fn is_symbol(c: char) -> bool {
        c.is_alphanumeric() || "+-*/<=>!?&$%".contains(c)
    }
    
    fn parse_list(input: &str) -> IResult<&str, SExpr> {
        let (input, elements) = delimited(
            char('('),
            many0(delimited(multispace0, Self::parse_expr, multispace0)),
            char(')'),
        )(input)?;
        Ok((input, SExpr::List(elements)))
    }
    
    fn parse_expr(input: &str) -> IResult<&str, SExpr> {
        alt((Self::parse_atom, Self::parse_list))(input)
    }
    
    fn parse_sexpr(input: &str) -> IResult<&str, SExpr> {
        delimited(multispace0, Self::parse_expr, multispace0)(input)
    }
}


fn main() {

}
