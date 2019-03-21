#/*
#* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Lola-2 parser library.
#*
#* The following is the license that applies to this copy of the
#* library. For a license to use the library under conditions
#* other than those described here, please email to me@rochus-keller.ch.
#*
#* GNU General Public License Usage
#* This file may be used under the terms of the GNU General Public
#* License (GPL) versions 2.0 or 3.0 as published by the Free Software
#* Foundation and appearing in the file LICENSE.GPL included in
#* the packaging of this file. Please review the following information
#* to ensure GNU General Public Licensing requirements will be met:
#* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
#* http://www.gnu.org/copyleft/gpl.html.
#*/

INCLUDEPATH +=  ..

SOURCES += \
    $$PWD/LlSynTree.cpp \
    $$PWD/LlTokenType.cpp \
    $$PWD/LlParser.cpp \
    $$PWD/LlToken.cpp \
    $$PWD/LlLexer.cpp \
    $$PWD/LlFileCache.cpp \
    $$PWD/LlErrors.cpp \
    $$PWD/LlCrossRefModel.cpp

HEADERS  += \
    $$PWD/LlSynTree.h \
    $$PWD/LlTokenType.h \
    $$PWD/LlParser.h \
    $$PWD/LlToken.h \
    $$PWD/LlLexer.h \
    $$PWD/LlFileCache.h \
    $$PWD/LlErrors.h \
    $$PWD/LlCrossRefModel.h

