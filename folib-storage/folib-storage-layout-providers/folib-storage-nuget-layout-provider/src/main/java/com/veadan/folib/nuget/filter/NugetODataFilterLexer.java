// Generated from com/veadan/folib/nuget/filter/NugetODataFilter.g4 by ANTLR 4.7.1
package com.veadan.folib.nuget.filter;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNDeserializer;
import org.antlr.v4.runtime.atn.LexerATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class NugetODataFilterLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, TO_LOWER=4, TAG=5, ATTRIBUTE=6, EQ=7, GE=8, AND=9, 
		OR=10, NOT=11, VALUE=12, WHITESPACE=13;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"T__0", "T__1", "T__2", "TO_LOWER", "TAG", "ATTRIBUTE", "EQ", "GE", "AND", 
		"OR", "NOT", "VALUE", "WHITESPACE"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "')'", "'''", "'tolower'", "'IsLatestVersion'", null, "'eq'", 
		"'ge'", "'and'", "'or'", "'not'", null, "' '"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, "TO_LOWER", "TAG", "ATTRIBUTE", "EQ", "GE", "AND", 
		"OR", "NOT", "VALUE", "WHITESPACE"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}


	public NugetODataFilterLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "NugetODataFilter.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\17`\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\5\3\5"+
		"\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3"+
		"\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\5\7E\n\7\3\b\3\b\3\b\3"+
		"\t\3\t\3\t\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\r\6\rY\n\r"+
		"\r\r\16\rZ\3\16\3\16\3\16\3\16\2\2\17\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21"+
		"\n\23\13\25\f\27\r\31\16\33\17\3\2\3\7\2/\60\62;C\\aac|\2a\2\3\3\2\2\2"+
		"\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2"+
		"\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2"+
		"\2\33\3\2\2\2\3\35\3\2\2\2\5\37\3\2\2\2\7!\3\2\2\2\t#\3\2\2\2\13+\3\2"+
		"\2\2\rD\3\2\2\2\17F\3\2\2\2\21I\3\2\2\2\23L\3\2\2\2\25P\3\2\2\2\27S\3"+
		"\2\2\2\31X\3\2\2\2\33\\\3\2\2\2\35\36\7*\2\2\36\4\3\2\2\2\37 \7+\2\2 "+
		"\6\3\2\2\2!\"\7)\2\2\"\b\3\2\2\2#$\7v\2\2$%\7q\2\2%&\7n\2\2&\'\7q\2\2"+
		"\'(\7y\2\2()\7g\2\2)*\7t\2\2*\n\3\2\2\2+,\7K\2\2,-\7u\2\2-.\7N\2\2./\7"+
		"c\2\2/\60\7v\2\2\60\61\7g\2\2\61\62\7u\2\2\62\63\7v\2\2\63\64\7X\2\2\64"+
		"\65\7g\2\2\65\66\7t\2\2\66\67\7u\2\2\678\7k\2\289\7q\2\29:\7p\2\2:\f\3"+
		"\2\2\2;<\7K\2\2<E\7f\2\2=>\7X\2\2>?\7g\2\2?@\7t\2\2@A\7u\2\2AB\7k\2\2"+
		"BC\7q\2\2CE\7p\2\2D;\3\2\2\2D=\3\2\2\2E\16\3\2\2\2FG\7g\2\2GH\7s\2\2H"+
		"\20\3\2\2\2IJ\7i\2\2JK\7g\2\2K\22\3\2\2\2LM\7c\2\2MN\7p\2\2NO\7f\2\2O"+
		"\24\3\2\2\2PQ\7q\2\2QR\7t\2\2R\26\3\2\2\2ST\7p\2\2TU\7q\2\2UV\7v\2\2V"+
		"\30\3\2\2\2WY\t\2\2\2XW\3\2\2\2YZ\3\2\2\2ZX\3\2\2\2Z[\3\2\2\2[\32\3\2"+
		"\2\2\\]\7\"\2\2]^\3\2\2\2^_\b\16\2\2_\34\3\2\2\2\5\2DZ\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}