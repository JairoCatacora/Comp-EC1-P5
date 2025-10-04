#include <iostream>
#include <fstream>
#include <cmath>
#include "ast.h"
#include "visitor.h"


using namespace std;
unordered_map<std::string, int> memoria;

///////////////////////////////////////////////////////////////////////////////////
int BinaryExp::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int NumberExp::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int SqrtExp::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int Program::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int IdExp::accept(Visitor* visitor) {
    return visitor->visit(this);
}


int PrintStm::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int AssignStm::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int IfStm::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int WhileStm::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int SwitchStm::accept(Visitor* visitor) {
    return visitor->visit(this);
}

int NotExp::accept(Visitor* visitor) {
    return visitor->visit(this);
}

///////////////////////////////////////////////////////////////////////////////////

int PrintVisitor::visit(BinaryExp* exp) {
    exp->left->accept(this);
    cout << ' ' << Exp::binopToChar(exp->op) << ' ';
    exp->right->accept(this);
    return 0;
}

int PrintVisitor::visit(NumberExp* exp) {
    cout << exp->value;
    return 0;
}

int PrintVisitor::visit(SqrtExp* exp) {
    cout << "sqrt(";
    exp->value->accept(this);
    cout <<  ")";
    return 0;
}

int PrintVisitor::visit(NotExp*  exp) {
    cout << "not(";
    exp->e->accept(this);
    cout << ")";
    return 0;
}

void PrintVisitor::imprimir(Program* programa){
    if (programa)
    {
        cout << "Codigo:" << endl; 
        programa->accept(this);
        cout << endl;
    }
    return ;
}

///////////////////////////////////////////////////////////////////////////////////
int EVALVisitor::visit(BinaryExp* exp) {
    int result;
    int v1 = exp->left->accept(this);
    int v2 = exp->right->accept(this);
    switch (exp->op) {
        case MENOR_OP:
            result = v1 < v2;
            break;
        case MAYOR_OP:
            result = v1 > v2;
            break;
        case PLUS_OP:
            result = v1 + v2;
            break;
        case MINUS_OP:
            result = v1 - v2;
            break;
        case MUL_OP:
            result = v1 * v2;
            break;
        case DIV_OP:
            if (v2 != 0)
                result = v1 / v2;
            else {
                cout << "Error: división por cero" << endl;
                result = 0;
            }
            break;
        case POW_OP:
            result = pow(v1,v2);
            break;
        case AND_OP:
            result = v1 && v2;
            break;
        case OR_OP:
            result = v1 || v2;
            break;
        default:
            cout << "Operador desconocido" << endl;
            result = 0;
    }
    return result;
}

int EVALVisitor::visit(NumberExp* exp) {
    return exp->value;
}

int EVALVisitor::visit(SqrtExp* exp) {
    return floor(sqrt( exp->value->accept(this)));
}

int EVALVisitor::visit(NotExp* exp) {
    return ~exp->e->accept(this);
}

void EVALVisitor::interprete(Program* programa){
    if (programa)
    {
        cout << "Interprete:" << endl;
        programa->accept(this);
    }
    return;

}

//////////////////////////////////////////

int PrintVisitor::visit(Program* p) {
    for(auto i: p->slist){
        i->accept(this);
    }
    return 0;
}

int PrintVisitor::visit(PrintStm* stm) {
    cout << "print(";
    stm->e->accept(this);
    cout << ")" << endl;
    return 0;
}

int PrintVisitor::visit(AssignStm* stm) {
    cout << stm->id << "=";
    stm->e->accept(this);
    cout << endl;
    return 0;
}

int PrintVisitor::visit(IdExp* p) {
    cout << p->value;
    return 0;
}

int EVALVisitor::visit(Program* p) {
    for(auto i: p->slist){
        i->accept(this);
    }
    return 0;
}

int EVALVisitor::visit(PrintStm* p) {
    cout << p->e->accept(this) << endl; 
    return 0;
}

int EVALVisitor::visit(AssignStm* p) {
    memoria[p->id] = p->e->accept(this);
    return 0;
}

int EVALVisitor::visit(IdExp* p) {
    return memoria[p->value];
}



int EVALVisitor::visit(IfStm* stm) {
    if (stm->condicion->accept(this)){
        for(auto i:stm->slist1){
            i->accept(this);
        }
    }
    else{
        for(auto i:stm->slist2){
            i->accept(this);
        } 
    }
    return 0;
}

int EVALVisitor::visit(WhileStm* stm) {
    while(stm->condicion->accept(this)){
        for(auto i:stm->slist1){
            i->accept(this);
        }
    }
    return 0;
}

int EVALVisitor::visit(SwitchStm* stm) {
    int v = stm->e->accept(this);
    bool found = false;
    for (auto n : stm->cases) {
        int caseValue = n->accept(this);
        if (caseValue == v) {
            found = true;
            auto stmList = stm->slist.front();
            for (auto i : stmList) {
                i->accept(this);
            }
        }
    }
    if (!found) {
        for (auto i : stm->dfcase) {
            i->accept(this);
        }
    }
    return 0;
}



int PrintVisitor::visit(IfStm* stm) {
    cout << "if " ;
    stm->condicion->accept(this);
    cout  << " then" << endl;
    for (auto i:stm->slist1){
        i->accept(this);
    }
    if (stm->parteelse){
        cout << "else"  << endl;;
        for (auto i:stm->slist2){
             i->accept(this);
    }
    }   
    cout << "endif" << endl;
    return 0;
}

int PrintVisitor::visit(WhileStm* stm) {
    cout << "while " ;
    stm->condicion->accept(this);
    cout  << " do" << endl;
    for (auto i:stm->slist1){
        i->accept(this);
    }
    cout << "endwhile" << endl;
    return 0;
}

int PrintVisitor::visit(SwitchStm* stm) {
    cout << "switch ";
    stm->e->accept(this);
    for(auto i: stm->slist){
        cout << "case ";
        i->accept(this);
        cout << ":" << endl;
        
        for(auto j:i){
            j->accept(this);
        }
    }  
    cout << "default:" << endl;
    for (auto i : stm->dfcase) {
        i->accept(this);
    }
    cout << "}" << endl;
    return 0;
}