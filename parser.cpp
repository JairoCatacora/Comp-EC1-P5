#include <iostream>
#include <stdexcept>
#include "token.h"
#include "scanner.h"
#include "ast.h"
#include "parser.h"

using namespace std;

// =============================
// Métodos de la clase Parser
// =============================

Parser::Parser(Scanner* sc) : scanner(sc) {
    previous = nullptr;
    current = scanner->nextToken();
    if (current->type == Token::ERR) {
        throw runtime_error("Error léxico");
    }
}

bool Parser::match(Token::Type ttype) {
    if (check(ttype)) {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(Token::Type ttype) {
    if (isAtEnd()) return false;
    return current->type == ttype;
}

bool Parser::advance() {
    if (!isAtEnd()) {
        Token* temp = current;
        if (previous) delete previous;
        current = scanner->nextToken();
        previous = temp;

        if (check(Token::ERR)) {
            throw runtime_error("Error lexico");
        }
        return true;
    }
    return false;
}

bool Parser::isAtEnd() {
    return (current->type == Token::END);
}


// =============================
// Reglas gramaticales
// =============================

Program* Parser::parseProgram() {
    Program* p = new Program();
    p->add(parseStm());
    while(match(Token::SEMICOL)){
        p->add(parseStm());
    }
    if (!isAtEnd()) {
        throw runtime_error("Error sintáctico");
    }
    
    cout << "Parseo exitoso" << endl;
    return p;
}

Stm* Parser::parseStm() {
    Stm* a;
    Exp* e;
    string variable;
    if(match(Token::ID)){
        variable = previous->text;
        match(Token::ASSIGN);
        e = parseCE();
        return new AssignStm(variable,e);
    }
    else if(match(Token::PRINT)){
        match(Token::LPAREN);
        e = parseCE();
        match(Token::RPAREN);
        return new PrintStm(e);
    }
    else if(match(Token::WHILE)){
        e =  parseCE(); 
        WhileStm* clasewhile = new WhileStm(e);  
        match(Token::DO);
        clasewhile->slist1.push_back(parseStm());
        while(match(Token::SEMICOL)){
            clasewhile->slist1.push_back(parseStm());
        }
        match(Token::ENDWHILE);
        return clasewhile;
    }

    else if(match(Token::IF)){
        e =  parseCE(); 
        IfStm* claseif = new IfStm(e);  
        match(Token::THEN);
        claseif->slist1.push_back(parseStm());
        while(match(Token::SEMICOL)){
            claseif->slist1.push_back(parseStm());
        }
        if(match(Token::ELSE)){
           claseif->parteelse=true; 
            claseif->slist2.push_back(parseStm());
            while(match(Token::SEMICOL)){
                claseif->slist2.push_back(parseStm());
            }
        }
        match(Token::ENDIF);
        return claseif;
    }

    else{
        throw runtime_error("Error sintáctico");
    }
    return a;
}

Exp* Parser::parseCE() {
    Exp* l = parseBE();
    if (match(Token::LE)) {
        BinaryOp op = LE_OP;
        Exp* r = parseBE();
        l = new BinaryExp(l, r, op);
    }
    return l;
}


Exp* Parser::parseBE() {
    Exp* l = parseE();
    while (match(Token::PLUS) || match(Token::MINUS)) {
        BinaryOp op;
        if (previous->type == Token::PLUS){
            op = PLUS_OP;
        }
        else{
            op = MINUS_OP;
        }
        Exp* r = parseE();
        l = new BinaryExp(l, r, op);
    }
    return l;
}


Exp* Parser::parseE() {
    Exp* l = parseT();
    while (match(Token::MUL) || match(Token::DIV)) {
        BinaryOp op;
        if (previous->type == Token::MUL){
            op = MUL_OP;
        }
        else{
            op = DIV_OP;
        }
        Exp* r = parseT();
        l = new BinaryExp(l, r, op);
    }
    return l;
}


Exp* Parser::parseT() {
    Exp* l = parseF();
    if (match(Token::POW)) {
        BinaryOp op = POW_OP;
        Exp* r = parseF();
        l = new BinaryExp(l, r, op);
    }
    return l;
}

Exp* Parser::parseF() {
    Exp* e; 
    if (match(Token::NUM)) {
        return new NumberExp(stoi(previous->text));
    } 
    else if (match(Token::LPAREN))
    {
        e = parseCE();
        match(Token::RPAREN);
        return e;
    }
    else if (match(Token::SQRT))
    {   
        match(Token::LPAREN);
        e = parseCE();
        match(Token::RPAREN);
        return new SqrtExp(e);
    }
    else if (match(Token::ID))
    {   
        return new IdExp(previous->text);
    }
    else {
        throw runtime_error("Error sintáctico");
    }
}
