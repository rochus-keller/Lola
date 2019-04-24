/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Lola-2 parser library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include "LlCrossRefModel.h"
#include "LlSynTree.h"
#include "LlErrors.h"
#include "LlLexer.h"
#include "LlParser.h"
#include <QtDebug>
#include <QElapsedTimer>
#include <QThread>
#include <QBuffer>
#include <QFileInfo>
using namespace Ll;

//#define _DUMP_AST

// für QtConcurrent::run braucht man extra eine separate Library mit minimalem Mehrwert
class CrossRefModel::Worker : public QThread
{
public:
    Worker(CrossRefModel* p):QThread(p){}
    void run()
    {
        //qDebug() << "CrossRefModel::Worker start";
        CrossRefModel* mdl = static_cast<CrossRefModel*>(parent());
        runUpdater(mdl);
        //qDebug() << "CrossRefModel::Worker end";
    }
};

CrossRefModel::CrossRefModel(QObject *parent, FileCache* fc) : QObject(parent)
{
    d_worker = new Worker(this);
    connect(d_worker,SIGNAL(finished()), this, SLOT(onWorkFinished()) );

    d_fcache = fc;

    d_errs = new Errors(this);
    d_errs->setShowWarnings(false);
}

CrossRefModel::~CrossRefModel()
{
    d_break = 1;
    d_worker->wait();
}

bool CrossRefModel::updateFiles(const QStringList& files, bool synchronous)
{
    d_lock.lockForWrite();
    foreach( const QString& f, files )
    {
        if( !d_work.contains(f) )
            d_work.append(f);
    }
    d_lock.unlock();

    if( !d_worker->isRunning() )
        d_worker->start();
    if( synchronous )
        d_worker->wait();
    return true;
}

bool CrossRefModel::parseString(const QString& code, const QString& sourcePath)
{
    d_lock.lockForWrite();

    ScopeRefList scopes;

    Errors errs(0,true);
    errs.setShowWarnings(false);
    errs.setReportToConsole(false);
    errs.setRecord(true);

    QBuffer in;
    in.setData( code.toLatin1() );
    in.open(QIODevice::ReadOnly);
    parseStream( &in, sourcePath, scopes, &errs, d_fcache );
    insertFiles( QStringList() << sourcePath, scopes, &errs, false );

    d_lock.unlock();
    return errs.getErrCount() == 0;
}

void CrossRefModel::clear()
{
    QSet<QString> files;
    d_lock.lockForWrite();
    foreach( const QString& f, d_work )
        files.insert(f);
    d_work.clear();
    foreach( const SymRef& sub, d_global.d_children )
    {
        files.insert(sub->d_tok.d_sourcePath);
    }
    d_global.d_children.clear();
    d_global.d_names.clear();
    d_index.clear();
    d_revIndex.clear();
    d_lock.unlock();
    emit sigModelUpdated();
    foreach( const QString& file, files )
        emit sigFileUpdated(file);

}

bool CrossRefModel::isEmpty() const
{
    d_lock.lockForRead();
    const bool res = d_global.d_children.isEmpty();
    d_lock.unlock();
    return res;
}

static bool hitsArea( const CrossRefModel::SymRef& sub, quint32 line, quint16 col, const QString& source )
{
    // es wurde in isHit bereits verifiziert, dass sub selber nicht getroffen wurde
    if( sub->tok().d_sourcePath != source || sub->tok().d_lineNr > line ||
            ( sub->tok().d_lineNr == line && sub->tok().d_colNr > col ) )
        return false;
    if( sub->children().isEmpty() )
        return false;
    foreach( const CrossRefModel::SymRef& subsub, sub->children() )
    {
        if( subsub->tok().d_sourcePath == source &&
                ( line < subsub->tok().d_lineNr || line == subsub->tok().d_lineNr && col <= subsub->tok().d_colNr ) )
            return true;
    }
    return false;
}

CrossRefModel::TreePath CrossRefModel::findSymbolBySourcePos(const QString& file, quint32 line, quint16 col,
                                                             bool onlyIdents , bool hitEmpty) const
{
    SymRefList list;
    d_lock.lockForRead();
    list = d_global.children();
    d_lock.unlock();

    TreePath res;
    foreach( const SymRef& sub, list )
    {
        if( sub->d_tok.d_sourcePath == file )
        {
            res.push_front( sub );
            if( findSymbolBySourcePosImp( res,line,col,onlyIdents,hitEmpty) )
                return res;
            if( hitEmpty && hitsArea( sub, line, col, file ) )
                return res;
            res.pop_front();
        }
    }
    return res;
}

CrossRefModel::IdentDeclRef CrossRefModel::findDeclarationOfSymbolAtSourcePos(const QString& file, quint32 line, quint16 col) const
{
    TreePath path = findSymbolBySourcePos( file, line, col );
    if( path.isEmpty() )
        return IdentDeclRef();
    else
        return findDeclarationOfSymbol(path.first().data());
}

CrossRefModel::IdentDeclRef CrossRefModel::findDeclarationOfSymbol(const CrossRefModel::Symbol* sym) const
{
    d_lock.lockForRead();
    IdentDeclRef res(const_cast<IdentDecl*>(d_index.value(sym))); // es gibt eh nur const-Methoden
    d_lock.unlock();
    return res;
}

CrossRefModel::SymRefList CrossRefModel::findAllReferencingSymbols(const CrossRefModel::Symbol* sym) const
{
    SymRefList res;
    d_lock.lockForRead();
    QList<const Symbol*> tmp = d_revIndex.values( sym );
    foreach( const Symbol* s, tmp )
        res.append( SymRef(const_cast<Symbol*>(s)) );
    d_lock.unlock();
    return res;
}

CrossRefModel::SymRefList CrossRefModel::findReferencingSymbolsByFile(const CrossRefModel::Symbol* sym, const QString& file) const
{
    SymRefList res;
    d_lock.lockForRead();
    QList<const Symbol*> tmp = d_revIndex.values( sym );
    foreach( const Symbol* s, tmp )
    {
        if( s->d_tok.d_sourcePath == file )
            res.append( SymRef(const_cast<Symbol*>(s)) );
    }
    d_lock.unlock();
    return res;
}

CrossRefModel::SymRef CrossRefModel::findGlobal(const QByteArray& name) const
{
    SymRef res;
    d_lock.lockForRead();
    const CrossRefModel::IdentDecl* decl = findNameInScope( &d_global, name, false);
    if( decl )
        res = decl->decl();
    d_lock.unlock();
    return res;
}

CrossRefModel::SymRefList CrossRefModel::getGlobalSyms(const QString& file) const
{
    SymRefList res;
    d_lock.lockForRead();
    if( file.isEmpty() )
        res = d_global.d_children;
    else
    {
        foreach( const SymRef& sym, d_global.d_children )
        {
            if( sym->d_tok.d_sourcePath == file )
                res.append(sym);
        }
    }
    d_lock.unlock();
    return res;
}

CrossRefModel::IdentDeclRefList CrossRefModel::getGlobalNames(const QString& file) const
{
    IdentDeclRefList res;
    d_lock.lockForRead();

    Scope::Names::const_iterator i;
    for( i = d_global.d_names.begin(); i != d_global.d_names.end(); ++i )
    {
        if( file.isEmpty() || file == i.value()->tok().d_sourcePath )
            res.append( IdentDeclRef( i.value() ) );
    }
    d_lock.unlock();
    return res;
}

QList<Token> CrossRefModel::findTokenByPos(const QString& line, int col, int* pos, bool supportSv)
{
    Lexer l;
    l.setIgnoreComments(false);
    l.setPackComments(false);
    //l.setSupportSvExt(supportSv);

    QList<Token> res = l.tokens(line);
    if( pos )
    {
        *pos = -1;
        for( int i = 0; i < res.size(); i++ )
        {
            const Token& t = res[i];
            if( t.d_colNr <= col && col < ( t.d_colNr + t.d_len ) )
            {
                *pos = i;
            }
        }
    }
    return res;
}

QString CrossRefModel::qualifiedName(const CrossRefModel::TreePath& path, bool skipFirst)
{
    return qualifiedNameParts(path,skipFirst).join(QChar('.') );
}

QStringList CrossRefModel::qualifiedNameParts(const CrossRefModel::TreePath& path, bool skipFirst)
{
    QStringList res;
    for( int i = path.size() - 1; i >= 0; i-- )
    {
        QByteArray name = path[i]->d_tok.d_val;

        if( !name.isEmpty() && ( i != 0 || !skipFirst ) )
        {
            res << QString::fromLatin1( name );
        }
    }
    return res;
}

const CrossRefModel::Scope*CrossRefModel::closestScope(const CrossRefModel::TreePath& path)
{
    for( int i = 0; i < path.size(); i++ )
    {
        Q_ASSERT(path[i]);
        const Scope* s = path[i]->toScope();
        if( s )
            return s;
    }
    return 0;
}

const CrossRefModel::Branch*CrossRefModel::closestBranch(const CrossRefModel::TreePath& path)
{
    for( int i = 0; i < path.size(); i++ )
    {
        Q_ASSERT(path[i]);
        const Branch* s = path[i]->toBranch();
        if( s )
            return s;
    }
    return 0;
}

const CrossRefModel::IdentDecl* CrossRefModel::findNameInScope(const Scope* scope, const QByteArray& name, bool recursiv, bool ports)
{
    const IdentDecl* res = scope->d_names.value(name);
    if( res )
        return res;
    if( scope->d_super && recursiv )
        return findNameInScope( scope->d_super->toScope(), name, recursiv, ports );
    else
        return 0;
}

static void findTokensOnSameLine( QList<const SynTree*>& res, const SynTree* st, int line, const QString& path )
{
    foreach( const SynTree* sub, st->d_children )
    {
        if( sub->d_tok.d_sourcePath == path && sub->d_tok.d_lineNr > quint32(line))
            return;
        if( sub->d_tok.d_type < SynTree::R_First )
            res << sub;
        else
            findTokensOnSameLine(res, sub, line, path );
    }
}

quint16 CrossRefModel::calcTextLenOfDecl(const SynTree* st)
{
    QList<const SynTree*> tokens;
    findTokensOnSameLine( tokens, st, st->d_tok.d_lineNr, st->d_tok.d_sourcePath );
    int i = 0;
    while( i < tokens.size() )
    {
        const SynTree* cur = tokens[i];
        switch( cur->d_tok.d_type )
        {
        case Tok_Semi:
        case Tok_Hash:
        case Tok_Lpar:
        case Tok_identifier:
            return cur->d_tok.d_colNr - st->d_tok.d_colNr;
        case Tok_Lbrack:
            {
                i++;
                int level = 1;
                while( i < tokens.size() )
                {
                    if( tokens[i]->d_tok.d_type == Tok_Rbrack )
                    {
                        level--;
                        if(level==0)
                            break;
                    }else if( tokens[i]->d_tok.d_type == Tok_Lbrack )
                        level++;
                    i++;
                }
            }
            break;
        default:
            break;
        }
        i++;
    }
    if( !tokens.isEmpty() && tokenTypeIsKeyword( tokens.first()->d_tok.d_type) )
        return ::strlen( tokenTypeString( tokens.first()->d_tok.d_type ) );
    else
        return 0;
}

quint16 CrossRefModel::calcKeyWordLen(const SynTree* st)
{
    if( !st->d_children.isEmpty() && tokenTypeIsKeyword( st->d_children.first()->d_tok.d_type ) )
        return ::strlen( tokenTypeString( st->d_children.first()->d_tok.d_type ) );
    else
        return 0;
}

static inline bool isHit( const CrossRefModel::SymRef& sub, quint32 line, quint16 col, const QString& source )
{
    // Nur tatsächlich in der Source am Ort sichtbare Symbole treffen, also nicht d_substituted
    return sub->tok().d_lineNr == line &&
            sub->tok().d_colNr <= col && col <= ( sub->tok().d_colNr + sub->tok().d_len )
            && sub->tok().d_sourcePath == source;
}


bool CrossRefModel::findSymbolBySourcePosImp(CrossRefModel::TreePath& path, quint32 line, quint16 col,
                                             bool onlyIdents, bool hitEmpty )
{
    Q_ASSERT( !path.isEmpty() );

    // TODO: z.B. Binary search anwenden, da ja Idents alle nach Zeilen und Spalten geordnet sind
    foreach( const SymRef& sub, path.front()->children() )
    {
        path.push_front(sub);

        if( ( sub->d_tok.d_type == Tok_identifier || !onlyIdents ) &&
                isHit( sub, line, col, path.last()->d_tok.d_sourcePath ) )
            return true;

        if( findSymbolBySourcePosImp( path, line, col, onlyIdents, hitEmpty ) )
            return true;

        if( hitEmpty && hitsArea( sub, line, col, path.last()->d_tok.d_sourcePath ) )
            return true;

        //else
        path.pop_front();
    }
    return false;
}

void CrossRefModel::runUpdater(CrossRefModel* mdl)
{
    QStringList files;
    mdl->d_lock.lockForWrite();
    if( !mdl->d_work.isEmpty() )
    {
        files = mdl->d_work;
        mdl->d_work.clear();
    }
    mdl->d_lock.unlock();

    ScopeRefList scopes;

    Errors errs(0,true);
    errs.setShowWarnings(false);
    errs.setReportToConsole(false);
    errs.setRecord(true);

    if( mdl->d_break )
        return;
    parseFiles( files, scopes, &errs, mdl->d_fcache, &mdl->d_break );
    if( mdl->d_break )
        return;
    mdl->insertFiles( files, scopes, &errs );

//    qDebug() << files.size() << "updated" << mdl->d_global.d_names.size() << "global names"
//             << mdl->d_global.d_children.size() << "global children"
//             << mdl->d_index.size() << "index entries" << mdl->d_revIndex.size() << "revindex entries";
    // TEST
    /*
    IdentDeclRef d = findDeclarationOfSymbolAtSourcePos( files.first(), 169, 6 );
    if( d.data() )
        qDebug() << "Declaration" << d->tok().d_val << d->tok().d_lineNr << d->tok().d_colNr;
        */
}

void CrossRefModel::dump(const CrossRefModel::Symbol* node, int level, bool recursive )
{
    QByteArray str;
    if( node->d_tok.d_type == Tok_Invalid )
        level--;
    else if( node->d_tok.d_type < SynTree::R_First )
    {
        if( tokenTypeIsKeyword( node->d_tok.d_type ) )
            str = node->d_tok.d_val.toUpper();
        else if( node->d_tok.d_type > TT_Specials )
            str = SynTree::rToStr( node->d_tok.d_type ) + QByteArray(" ") +
                    QByteArray("\"") + node->d_tok.d_val + QByteArray("\"");
        else
            str = QByteArray("\"") + node->d_tok.getName() + QByteArray("\"");

    }else
        str = SynTree::rToStr( node->d_tok.d_type );
    if( node->d_tok.d_type != Tok_Invalid )
    {
        str += QByteArray("\t\t\t\t\t") + QFileInfo(node->d_tok.d_sourcePath).fileName().toUtf8() +
                ":" + QByteArray::number(node->d_tok.d_lineNr);
        qDebug() << QByteArray(level*3, ' ').data() << str.data();
    }
    if( recursive )
        foreach( const SymRef& sub, node->children() )
            dump( sub.constData(), level + 1 );
}

void CrossRefModel::onWorkFinished()
{
    d_lock.lockForRead();
    bool runAgain = !d_work.isEmpty();
    d_lock.unlock();
    if( runAgain )
        d_worker->start();
}

CrossRefModel::ScopeRefNc CrossRefModel::createAst(const SynTree* top, Errors* err)
{
    ScopeRefNc res( new Scope() );
    res->d_tok = top->d_tok;
    SynTreePath synTree;
    synTree.push_front(top);
    fillAst( res.data(), res.data(), synTree, err );
    return res;
}

static bool hasScope(const SynTree* st)
{
    switch( st->d_tok.d_type )
    {
    case SynTree::R_module:
    case SynTree::R_ModuleType:
        return true;
    default:
        return false;
    }
}

static bool isDecl( quint16 type )
{
    switch( type )
    {
    case SynTree::R_ConstDeclaration:
    case SynTree::R_TypeDeclaration:
    case SynTree::R_varlist:
    case SynTree::R_VarRegDeclaration:
        return true;
    default:
        return false;
    }
}

void CrossRefModel::fillAst(Branch* parentAst, Scope* superScope, SynTreePath& synPath , Errors* err)
{
    Q_ASSERT( parentAst != 0 && superScope != 0 && !synPath.isEmpty() );

    Token label;
    const SynTree* parent = synPath.front();
    for( int i = 0; i < parent->d_children.size(); i++ )
    {
        const SynTree* child = parent->d_children[i];

        if( child->d_tok.d_type == SynTree::R_assignment_or_instantiation && child->d_children.size() > 2 )
        {
            // designator ( ':=' | '(' )
            if( child->d_children[1]->d_tok.d_type == Tok_ColonEq )
                parent->d_children[i]->d_tok.d_type = SynTree::R_assignment;
            else
                parent->d_children[i]->d_tok.d_type = SynTree::R_instantiation;
        }

        if( hasScope(child) )
        {
            Scope* curScope = new Scope();
            curScope->d_super = superScope;
            curScope->d_tok = child->d_tok;
            curScope->d_tok.d_len = calcKeyWordLen(child);
            parentAst->d_children.append( SymRef(curScope) );

            synPath.push_front(child);
            fillAst( curScope, curScope, synPath, err );
            synPath.pop_front();
        }else if( isDecl(child->d_tok.d_type) || child->d_tok.d_type == SynTree::R_designator )
        {
            Branch* curParent = new Branch();
            curParent->d_tok = child->d_tok;
            curParent->d_super = parentAst;
            parentAst->d_children.append( SymRef(curParent) );

            synPath.push_front(child);
            fillAst( curParent, superScope, synPath, err );
            synPath.pop_front();
        }else if( child->d_tok.d_type > SynTree::R_First )
        {
            // Gehe trotzdem runter da dort noch Idents sein können
            synPath.push_front(child);
            fillAst( parentAst, superScope, synPath, err );
            synPath.pop_front();
        }else if( child->d_tok.d_type == Tok_END )
        {
            // we need at least the last symbol of the production to be able to do
            // line/col based searches when the cursor is in white space
            parentAst->d_children.append( SymRef( new Symbol(child->d_tok) ) );
        }else if( child->d_tok.d_type == Tok_identifier )
        {

            Scope* scope = superScope;
            SymRefNc sym;

            const int pt = parent->d_tok.d_type;
            if( pt == SynTree::R_module )
            {
                // hier kommen nur Idents an, die sich unmittelbar unter der Scope Production befinden
                Q_ASSERT( parent->d_children.size() == 4 ); // MODULE identifier unit identifier
                if( child == parent->d_children[1] )
                {
                    sym = new IdentDecl();
                    scope = const_cast<Scope*>( superScope->d_super->toScope() );
                    superScope->d_tok.d_val = child->d_tok.d_val; // damit R_module gleich seinen Namen kennt
                    superScope->d_tok.d_lineNr = child->d_tok.d_lineNr;
                    superScope->d_tok.d_colNr = child->d_tok.d_colNr;
                }
            }else if( pt == SynTree::R_TypeDeclaration )
            {
                Q_ASSERT( parent->d_children.last()->d_tok.d_type == SynTree::R_type );
                Q_ASSERT( !parent->d_children.last()->d_children.isEmpty() &&
                          parent->d_children.last()->d_children.last()->d_tok.d_type == SynTree::R_SimpleType );
                Q_ASSERT( !parent->d_children.last()->d_children.last()->d_children.isEmpty() );
                const SynTree* mod = parent->d_children.last()->d_children.last()->d_children.first();
                if( mod->d_tok.d_type == SynTree::R_ModuleType && !mod->d_children.isEmpty() &&
                        mod->d_children.last()->d_tok.d_type == SynTree::R_unit &&
                        !mod->d_children.last()->d_children.isEmpty() &&
                        mod->d_children.last()->d_children.last()->d_tok.d_type == Tok_Hat )
                {
                    parentAst->d_tok.d_val = child->d_tok.d_val; // wenn ^
                    sym = new ModRef();
                }else
                    sym = new IdentDecl();
            }else if( pt == SynTree::R_ConstDeclaration || pt == SynTree::R_varlist )
            {
                sym = new IdentDecl();
            }

            if( sym.constData() == 0 )
                sym = new IdentUse();

            sym->d_tok = child->d_tok;
            parentAst->d_children.append(sym);
            if( IdentDecl* id = const_cast<IdentDecl*>( sym->toIdentDecl() ) )
            {
                id->d_decl = parentAst;
                const IdentDecl*& slot = scope->d_names[id->d_tok.d_val];
                if( slot != 0 )
                    err->error(Errors::Semantics, id->d_tok.d_sourcePath, id->d_tok.d_lineNr,id->d_tok.d_colNr,
                                  tr("duplicate name: %1").arg(id->d_tok.d_val.data()) );
                else
                    slot = id;
            }
        }
        label = Token();
    }
}

CrossRefModel::Symbol::Symbol(const Token& t):d_tok(t)
{

}

const CrossRefModel::Symbol::SymRefList& CrossRefModel::Symbol::children() const
{
    static SymRefList dummy;
    return dummy;
}

const CrossRefModel::Branch*CrossRefModel::Symbol::toBranch() const
{
    // more efficient than dynamic_cast or typeid, see ISO/IEC TR 18015:2004 5.3.8
    switch( getType() )
    {
    case ClassScope:
    case ClassBranch:
        return static_cast<const Branch*>(this);
    default:
        return 0;
    }
}

const CrossRefModel::Scope*CrossRefModel::Symbol::toScope() const
{
    if( getType() == ClassScope )
        return static_cast<const Scope*>(this);
    else
        return 0;
}

const CrossRefModel::Scope*CrossRefModel::Symbol::toScope(const CrossRefModel::Symbol* sym)
{
    if( sym == 0 )
        return 0;
    else
        return sym->toScope();
}

const CrossRefModel::IdentDecl*CrossRefModel::Symbol::toIdentDecl() const
{
    if( getType() == ClassIdentDecl || getType() == ClassModRef )
        return static_cast<const IdentDecl*>(this);
    else
        return 0;
}

const CrossRefModel::IdentUse*CrossRefModel::Symbol::toIdentUse() const
{
    if( getType() == ClassIdentUse )
        return static_cast<const IdentUse*>(this);
    else
        return 0;
}

const CrossRefModel::PathIdent*CrossRefModel::Symbol::toPathIdent() const
{
    if( getType() == ClassPathIdent )
        return static_cast<const PathIdent*>(this);
    else
        return 0;
}

const CrossRefModel::ModRef*CrossRefModel::Symbol::toModRef() const
{
    if( getType() == ClassModRef )
        return static_cast<const ModRef*>(this);
    else
        return 0;
}

const char*CrossRefModel::Symbol::getTypeName() const
{
    switch( getType() )
    {
    case ClassSymbol:
        return "Symbol";
    case ClassBranch:
        return "Branch";
    case ClassIdentDecl:
        return "IdentDecl";
    case ClassIdentUse:
        return "IdentUse";
    case ClassPathIdent:
        return "PathIdent";
    case ClassScope:
        return "Scope";
    case ClassModRef:
        return "ModRef";
    default:
        return "Unknown Type";
    }
}

const CrossRefModel::SymRefList& CrossRefModel::Branch::children() const
{
    return d_children;
}

const CrossRefModel::Symbol* CrossRefModel::findFirst(const Branch* b, quint16 type)
{
    foreach( const SymRef& sub, b->d_children )
    {
        if( sub && sub->d_tok.d_type == type )
            return sub.data();
    }
    return 0;
}

#ifdef _DUMP_AST
#include <typeinfo>
#include <cxxabi.h>
static void dumpAst(const CrossRefModel::Symbol* l, int level)
{
    int status;
    char * demangled = abi::__cxa_demangle(typeid(*l).name(),0,0,&status);
    qDebug() << QByteArray(level*3,' ').data() << SynTree::rToStr(l->tok().d_type) << l->tok().d_lineNr
             << l->tok().d_colNr << l->tok().d_len << l->tok().d_val << "\t\t" << demangled+19;
    free(demangled);
    foreach( const CrossRefModel::SymRef& sub, l->children() )
        dumpAst( sub.constData(), level+1 );
}
#endif

int CrossRefModel::parseFiles(const QStringList& files, ScopeRefList& scopes,
                               Errors* errs, FileCache* fcache, QAtomicInt* stop )
{
    QElapsedTimer t;
    t.start();
    int esum = 0;
    foreach( const QString& file, files )
    {
        if( stop && *stop )
            return esum;
        int e = errs->getErrCount();
        int w = errs->getWrnCount();
        //qDebug() << "start parseFiles" << file;
        parseStream( 0, file, scopes, errs, fcache );
        //qDebug() << "end parseFiles" << file;
        e = errs->getErrCount() - e;
        w = errs->getWrnCount() - w;
//        if( errs->reportToConsole() )
//        {
//            if( e != 0 && w != 0 )
//                qDebug() << "####" << e << "Errors and" << w << "Warnings in" << file;
//            else if( errs->showWarnings() && w != 0 )
//                qDebug() << "####" << w << "Warnings in" << file;
//            else if( e != 0)
//                qDebug() << "####" << e << "Errors in" << file;
//        }
        esum += e;
    }
//    if( errs->reportToConsole() )
//        qDebug() << "### Parsed" << files.size() << "files in" << t.elapsed() << "ms with" << esum << "errors";
    return esum;
}

bool CrossRefModel::parseStream(QIODevice* stream, const QString& sourcePath, CrossRefModel::ScopeRefList& refs, Errors* errs, FileCache* fcache)
{
    Lexer lex;
    lex.setErrors( errs );
    lex.setCache(fcache);

    lex.setStream( stream, sourcePath );

    Parser p( &lex, errs );
    int errCount = errs->getErrCount();
    p.RunParser();
    errCount = errs->getErrCount() - errCount;

    if( true ) // res ) // we need a SynTree in any case even with syntax errors
    {
        SynTree root;
        root.d_children = p.d_root.d_children;
        p.d_root.d_children.clear();

        ScopeRef top = createAst( &root, errs );
//        qDebug() << "*** parsed" << sourcePath;
//        foreach( const SymRef& sym, top->children() )
//            qDebug() << sym->d_tok.d_val << sym->d_tok.d_lineNr;
        refs.append(top);

#ifdef _DUMP_AST
        qDebug() << "********** dumping" << top->tok().d_sourcePath;
        dumpAst(top.constData(), 0 );
        qDebug() << "********** end dump" << top->tok().d_sourcePath;
#endif
//        qDebug() << "************** name table of" << file;
//        Scope::Names::const_iterator n;
//        for( n = top->d_names.begin(); n != top->d_names.end(); ++n )
//            qDebug() << n.key() << SynTree::rToStr(n.value()->d_tok.d_type) << n.value()->d_tok.d_lineNr;
    }
    return errCount == 0;
}

void CrossRefModel::insertFiles(const QStringList& files, const ScopeRefList& scopes, Errors* errs, bool lock )
{
    if( lock )
        d_lock.lockForRead();
    Scope newGlobal = d_global;
    if( lock )
        d_lock.unlock();

    QElapsedTimer t;
    t.start();

    int errCount = errs->getErrCount();

    // Lösche zuerst alles, was die neu geparsten Files betrifft, aus dem existierenden Global
    foreach( const QString& file, files )
        clearFile(&newGlobal,file);

    // scopes enthält für jedes geparste File einen Scope
    foreach( const ScopeRef& scope, scopes )
    {
        // Prüfe, ob die Namen in scope allenfalls schon existieren, ansonsten füge sie in Global
        Scope::Names::const_iterator scopeIter;
        for( scopeIter = scope->d_names.begin(); scopeIter != scope->d_names.end(); ++scopeIter )
        {
            Scope::Names::const_iterator globalIter = newGlobal.d_names.find( scopeIter.key() );
            if( globalIter != newGlobal.d_names.end() )
            {
                const IdentDecl* newDecl = scopeIter.value();
                const IdentDecl* existingDecl = globalIter.value();
                errs->error(Errors::Semantics, newDecl->d_decl->d_tok.d_sourcePath,
                            newDecl->d_decl->d_tok.d_lineNr, newDecl->d_decl->d_tok.d_colNr,
                              tr("duplicate cell name '%1' already declared in %2")
                            .arg( newDecl->d_tok.d_val.data() )
                            .arg( existingDecl->d_tok.d_sourcePath ) );
            }else
                newGlobal.d_names.insert( scopeIter.key(), scopeIter.value() );

        }
        // Alle children von scope werden übernommen und auf den neuen Global angepasst
        foreach( const SymRef& s, scope->children() )
        {
            Branch* b = const_cast<Branch*>( s->toBranch() );
            if( b )
                b->d_super = &newGlobal;
        }
        newGlobal.d_children += scope->d_children;

    }

    Index index;
    RevIndex revIndex;
    foreach( const SymRef& sub, newGlobal.d_children )
    {
        const Scope* cell = sub->toScope();
        // NOTE: the d_super of all scopes not in files still point to d_global! We don't care since this is
        // the only thread causing mutations and the mutations are serialized
        if( cell ) // sub may be 0!
            resolveIdents( index, revIndex, cell, 0, cell, &newGlobal, errs );
    }

    errCount = errs->getErrCount() - errCount;

//    if( errs->reportToConsole() )
//        qDebug() << "### Elaborated and indexed" << files.size() << "files in" << t.elapsed()
//                 << "ms with" << errCount << "errors";

    if( lock )
        d_lock.lockForWrite();
    t.restart();
    d_index = index;
    d_revIndex = revIndex;
    d_global.d_names = newGlobal.d_names;
    d_global.d_children = newGlobal.d_children;
    foreach( const SymRef& sub, d_global.d_children )
    {
        Scope* cell = const_cast<Scope*>( sub->toScope());
        if( cell ) // sub may be 0
            cell->d_super = &d_global;
    }
    d_errs->clearFiles(files);
    d_errs->update( *errs );
//    if( errs->reportToConsole() )
//        qDebug() << "### Replaced global scope in" << t.elapsed() << "ms";
    if( lock )
        d_lock.unlock();

    emit sigModelUpdated();
    foreach( const QString& file, files )
        emit sigFileUpdated(file);
}

void CrossRefModel::clearFile(Scope* global, const QString& file)
{
    SymRefList children = global->d_children;
    global->d_children.clear();
    for( int i = 0; i < children.size(); i++ )
    {
        if( children[i]->d_tok.d_sourcePath != file )
            global->d_children.append(children[i]);
    }
    Scope::Names::iterator i = global->d_names.begin();
    while( i != global->d_names.end() )
    {
        if( i.value()->d_tok.d_sourcePath == file )
            i = global->d_names.erase(i);
        else
            ++i;
    }
}

void CrossRefModel::resolveIdents(Index& index, RevIndex& revIndex, const Symbol* leaf, const Branch* parent,
                                  const Scope* curScope, const Scope* globScope, Errors* errs)
{
    Q_ASSERT( leaf != 0 );
    if( const IdentUse* use = leaf->toIdentUse() )
    {
        const IdentDecl* id = findNameInScope( curScope, use->d_tok.d_val, true, true );
        if( id != 0 )
        {
            index.insert( use, id );
            revIndex.insert( id, use );
        }else
            errs->error(Errors::Semantics, use->d_tok.d_sourcePath, use->d_tok.d_lineNr, use->d_tok.d_colNr,
                          tr("unknown identifier: %1").arg(use->d_tok.d_val.data()) );
    }else if( const PathIdent* use = leaf->toPathIdent() )
    {
        // Jeder hierarchical_identifier/_x enthält als children die ganze Kette von Idents
        if( use == parent->d_children.first().data() )
        {
            // Wir lösen die Kette nur beim ersten Ident auf
            QList<const PathIdent*> path;
            path << use;
            for(int i = 1; i < parent->d_children.size(); i++ )
            {
                const PathIdent* p = parent->d_children[i]->toPathIdent();
                if( p )
                    path << p;
            }
            const Scope* scope = curScope;
            for( int i = 0; i < path.size(); i++ )
            {
                //if( path[i]->d_tok.d_val == "s" )
                //    qDebug() << "hit" << path[i]->d_tok.d_lineNr << path[i]->d_tok.d_colNr;

                const IdentDecl* id = findNameInScope( scope, path[i]->d_tok.d_val, true, true );
                if( id != 0 )
                {
                    index.insert( path[i], id );
                    revIndex.insert( id, path[i] );
                    if( i < path.size() - 1 )
                    {
                        scope = 0;
                        Q_ASSERT( id->d_decl != 0 );
                        scope = id->d_decl->toScope();
                        if( scope == 0 )
                        {
                            errs->error(Errors::Semantics, id->d_tok.d_sourcePath, id->d_tok.d_lineNr, id->d_tok.d_colNr,
                                          tr("identifier is not a name space: %1").arg(id->d_tok.d_val.data()) );
                            break;
                        }
                    }
                }else
                    errs->error(Errors::Semantics, path[i]->d_tok.d_sourcePath, path[i]->d_tok.d_lineNr, path[i]->d_tok.d_colNr,
                                  tr("unknown identifier: %1").arg(path[i]->d_tok.d_val.data()) );
            }
        }
    }else if( const ModRef* ref = leaf->toModRef() )
    {
        const IdentDecl* cellId = findNameInScope( globScope, ref->d_tok.d_val, false, false );
        if( cellId != 0 )
        {
            index.insert( ref, cellId );
            revIndex.insert( cellId, ref );
        }else
            errs->error(Errors::Elaboration, ref->d_tok.d_sourcePath, ref->d_tok.d_lineNr, ref->d_tok.d_colNr,
                          tr("unknown module: %1").arg(ref->d_tok.d_val.data()) );
    }
    foreach( const SymRef& sub, leaf->children() )
    {
        const Scope* scope = sub->toScope();
        if( scope == 0 )
            scope = curScope;
        resolveIdents(index, revIndex, sub.data(), leaf->toBranch(), scope, globScope, errs );
    }
}

CrossRefModel::Branch::Branch():d_super(0)
{

}

CrossRefModel::Scope::Names2 CrossRefModel::Scope::getNames2(bool recursive) const
{
    Names2 res;
    const Scope* super = d_super ? d_super->toScope() : 0;
    if( recursive && super != 0 )
        res = super->getNames2(recursive);

    Names::const_iterator i;
    for( i = d_names.begin(); i != d_names.end(); ++i )
        res.insert( i.key(), IdentDeclRef(i.value()) );

    return res;
}

CrossRefModel::IdentDeclRefList CrossRefModel::Scope::getNames() const
{
    IdentDeclRefList res;
    Names::const_iterator i;
    for( i = d_names.begin(); i != d_names.end(); ++i )
        res.append( IdentDeclRef(i.value()) );
    return res;
}
