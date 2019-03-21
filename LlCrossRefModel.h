#ifndef LACROSSREFMDL_H
#define LACROSSREFMDL_H

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

#include <QObject>
#include <QSharedData>
#include <QMap>
#include <QReadWriteLock>
#include <QStringList>
#include <Lola/LlToken.h>

namespace Ll
{
    class SynTree;
    class Errors;
    class Lexer;
    class FileCache;

    class CrossRefModel : public QObject
    {
        Q_OBJECT
    public:
        class Branch;
        class Scope;
        class IdentDecl;
        class IdentUse;
        class PathIdent;
        class ModRef;

        enum { ClassSymbol, ClassBranch, ClassIdentDecl, ClassIdentUse, ClassPathIdent, ClassScope, ClassModRef };

        class Symbol : public QSharedData
        {
        public:
            typedef QExplicitlySharedDataPointer<const Symbol> SymRef;
            typedef QList<SymRef> SymRefList;

            Symbol(const Token& = Token() );

            const Token& tok() const { return d_tok; }
            virtual const SymRefList& children() const;
            const Branch* toBranch() const;
            const Scope* toScope() const;
            static const Scope* toScope(const Symbol*);
            const IdentDecl* toIdentDecl() const;
            const IdentUse* toIdentUse() const;
            const PathIdent* toPathIdent() const;
            const ModRef* toModRef() const;
            virtual int getType() const { return ClassSymbol; }
            const char* getTypeName() const;
        protected:
            virtual ~Symbol() {}

            friend class QExplicitlySharedDataPointer<Symbol>;
            friend class QExplicitlySharedDataPointer<const Symbol>;

        private:
            friend class CrossRefModel;
            Token d_tok;
        };

        typedef QExplicitlySharedDataPointer<const Symbol> SymRef;
        typedef QList<SymRef> SymRefList;
        typedef SymRefList TreePath; // index 0: bottom, index max: root

        class Branch : public Symbol
        {
        public:
            Branch();
            Branch* super() const { return d_super; }
            virtual const SymRefList& children() const Q_DECL_OVERRIDE;
            // TODO: Position des End-Tokens der Deklaration zwecks Kurzdarstellung bei Hover
            int getType() const Q_DECL_OVERRIDE { return ClassBranch; }
        private:
            friend class CrossRefModel;
            Branch* d_super; // Points to enclosing scope (for Scopes) and to super Branch (for decls)
            SymRefList d_children;
        };

        class IdentDecl : public Symbol
        {
        public:
            const Branch* decl() const { return d_decl; }
            int getType() const Q_DECL_OVERRIDE { return ClassIdentDecl; }
        protected:
            IdentDecl():d_decl(0){}
        private:
            friend class CrossRefModel;
            Branch* d_decl; // points to Decl for Decl Idents, to containing AstNode for all others
        };
        typedef QExplicitlySharedDataPointer<const IdentDecl> IdentDeclRef;
        typedef QList<IdentDeclRef> IdentDeclRefList;

        class IdentUse : public Symbol
        {
        public:
            int getType() const Q_DECL_OVERRIDE { return ClassIdentUse; }
        };

        class PathIdent : public Symbol // Ident is part of hierarchical path
        {
        public:
            int getType() const Q_DECL_OVERRIDE { return ClassPathIdent; }
        };

        class ModRef : public IdentDecl // Name is a local declaration and a reference to a module
        {
        public:
            int getType() const Q_DECL_OVERRIDE { return ClassModRef; }
        };

        class Scope : public Branch
        {
        public:
            typedef QMap<QByteArray,IdentDeclRef> Names2;
            Names2 getNames2(bool recursive = true) const;
            IdentDeclRefList getNames() const;
        protected:
            int getType() const Q_DECL_OVERRIDE { return ClassScope; }
        private:
            friend class CrossRefModel;
            typedef QMap<QByteArray,const IdentDecl*> Names; // TODO: Hash
            Names d_names;
        };
        typedef QExplicitlySharedDataPointer<const Scope> ScopeRef;

        explicit CrossRefModel(QObject *parent = 0, FileCache* = 0);
        ~CrossRefModel();

        bool updateFiles( const QStringList&, bool synchronous = false );
        bool parseString( const QString& code, const QString& sourcePath = QString() );
        void clear();

        Errors* getErrs() const { return d_errs; }
        FileCache* getFcache() const { return d_fcache; }

        bool isEmpty() const;

        TreePath findSymbolBySourcePos(const QString& file, quint32 line, quint16 col ,
                                       bool onlyIdents = true, bool hitEmpty = false ) const;
        IdentDeclRef findDeclarationOfSymbolAtSourcePos(const QString& file, quint32 line, quint16 col) const;
        IdentDeclRef findDeclarationOfSymbol(const Symbol* ) const;
        SymRefList findAllReferencingSymbols(const Symbol* ) const;
        SymRefList findReferencingSymbolsByFile(const Symbol*, const QString& file ) const;
        SymRef findGlobal( const QByteArray& name ) const;
        SymRefList getGlobalSyms( const QString& file = QString() ) const;
        IdentDeclRefList getGlobalNames( const QString& file = QString() ) const;
        static QList<Token> findTokenByPos(const QString& line, int col, int* pos, bool supportSv = false );
        static QString qualifiedName( const TreePath&, bool skipFirst = false );
        static QStringList qualifiedNameParts( const TreePath&, bool skipFirst = false );
        static const Scope* closestScope( const TreePath& );
        static const Branch* closestBranch( const TreePath& );
        static void dump(const Symbol*, int level = 0, bool recursive = true);

    signals:
        void sigFileUpdated( const QString& path );
        void sigModelUpdated();

    protected:
        typedef QList<const SynTree*> SynTreePath; // top = last
        typedef QList<ScopeRef> ScopeRefList;
        typedef QHash<const Symbol*,const IdentDecl*> Index; // ident use -> ident declaration
        typedef QMultiHash<const Symbol*, const Symbol*> RevIndex;
        typedef QExplicitlySharedDataPointer<Scope> ScopeRefNc;
        typedef QExplicitlySharedDataPointer<Symbol> SymRefNc;

        static ScopeRefNc createAst( const SynTree*, Errors* ); // returns a global scope
        static void fillAst( Branch* parentAst, Scope* superScope, SynTreePath& synPath, Errors* );
        static const Symbol* findFirst(const Branch*, quint16 type);

        static int parseFiles(const QStringList& files, ScopeRefList&, Errors* errs, FileCache* fcache, QAtomicInt* );
        static bool parseStream(QIODevice* stream, const QString& sourcePath, ScopeRefList&,
                              Errors* errs, FileCache* fcache);
        void insertFiles(const QStringList& files, const ScopeRefList&, Errors* errs , bool lock = true); // write lock
        static void clearFile(Scope*, const QString& file);
        static void resolveIdents( Index&, RevIndex&, const Symbol*, const Branch*, const Scope*, const Scope*, Errors* );
        static const IdentDecl* findNameInScope( const Scope*, const QByteArray& name, bool recursiv = true, bool ports = false );
        static quint16 calcTextLenOfDecl( const SynTree* );
        static quint16 calcKeyWordLen( const SynTree* );
        static bool findSymbolBySourcePosImp(TreePath& path, quint32 line, quint16 col, bool onlyIdents , bool hitEmpty);
        static void runUpdater(CrossRefModel* );
    protected slots:
        void onWorkFinished();
    private:
        Errors* d_errs;
        FileCache* d_fcache;

        Scope d_global;
        Index d_index;
        RevIndex d_revIndex;

        mutable QReadWriteLock d_lock;
        QStringList d_work; // no set because order may be relevant
        class Worker;
        Worker* d_worker;
        QAtomicInt d_break;
    };
}
Q_DECLARE_METATYPE(Ll::CrossRefModel::SymRef)

#endif // LACROSSREFMDL_H
