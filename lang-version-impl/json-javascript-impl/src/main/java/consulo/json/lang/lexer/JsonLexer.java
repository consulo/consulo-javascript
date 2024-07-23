/* The following code was generated by JFlex 1.4.4 on 15.02.16 12:22 */

package consulo.json.lang.lexer;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.language.ast.IElementType;
import consulo.language.lexer.LexerBase;

/**
 * This class is a scanner generated by
 * <a href="http://www.jflex.de/">JFlex</a> 1.4.4
 * on 15.02.16 12:22 from the specification file
 * <tt>R:/_github.com/consulo/consulo-javascript/json-javascript-impl/src/org/mustbe/consulo/json/lang/lexer/json.flex</tt>
 */
public class JsonLexer extends LexerBase {
    /** initial size of the lookahead buffer */
    private static final int ZZ_BUFFERSIZE = 16384;

    /** lexical states */
    public static final int YYINITIAL = 0;

    /**
     * ZZ_LEXSTATE[l] is the state in the DFA for the lexical state l
     * ZZ_LEXSTATE[l+1] is the state in the DFA for the lexical state l
     * at the beginning of a line
     * l is of the form l = 2*k, k a non negative integer
     */
    private static final int ZZ_LEXSTATE[] = {
        0, 0
    };

    /**
     * Translates characters to character classes
     */
    private static final String ZZ_CMAP_PACKED =
        "\11\5\1\3\1\16\1\0\1\3\1\10\16\5\4\0\1\3\1\0" +
            "\1\21\1\0\1\4\2\0\1\17\2\0\1\7\1\15\1\37\1\41" +
            "\1\13\1\6\1\11\7\1\2\1\1\40\6\0\4\2\1\14\1\2" +
            "\21\4\1\12\2\4\1\35\1\20\1\36\1\0\1\4\1\0\1\27" +
            "\3\2\1\25\1\26\5\4\1\30\1\4\1\32\3\4\1\23\1\31" +
            "\1\22\1\24\2\4\1\12\2\4\1\33\1\0\1\34\1\0\41\5" +
            "\2\0\4\4\4\0\1\4\2\0\1\5\7\0\1\4\4\0\1\4" +
            "\5\0\27\4\1\0\37\4\1\0\u013f\4\31\0\162\4\4\0\14\4" +
            "\16\0\5\4\11\0\1\4\21\0\130\5\5\0\23\5\12\0\1\4" +
            "\13\0\1\4\1\0\3\4\1\0\1\4\1\0\24\4\1\0\54\4" +
            "\1\0\46\4\1\0\5\4\4\0\202\4\1\0\4\5\3\0\105\4" +
            "\1\0\46\4\2\0\2\4\6\0\20\4\41\0\46\4\2\0\1\4" +
            "\7\0\47\4\11\0\21\5\1\0\27\5\1\0\3\5\1\0\1\5" +
            "\1\0\2\5\1\0\1\5\13\0\33\4\5\0\3\4\15\0\4\5" +
            "\14\0\6\5\13\0\32\4\5\0\13\4\16\5\7\0\12\5\4\0" +
            "\2\4\1\5\143\4\1\0\1\4\10\5\1\0\6\5\2\4\2\5" +
            "\1\0\4\5\2\4\12\5\3\4\2\0\1\4\17\0\1\5\1\4" +
            "\1\5\36\4\33\5\2\0\3\4\60\0\46\4\13\5\1\4\u014f\0" +
            "\3\5\66\4\2\0\1\5\1\4\20\5\2\0\1\4\4\5\3\0" +
            "\12\4\2\5\2\0\12\5\21\0\3\5\1\0\10\4\2\0\2\4" +
            "\2\0\26\4\1\0\7\4\1\0\1\4\3\0\4\4\2\0\1\5" +
            "\1\4\7\5\2\0\2\5\2\0\3\5\11\0\1\5\4\0\2\4" +
            "\1\0\3\4\2\5\2\0\12\5\4\4\15\0\3\5\1\0\6\4" +
            "\4\0\2\4\2\0\26\4\1\0\7\4\1\0\2\4\1\0\2\4" +
            "\1\0\2\4\2\0\1\5\1\0\5\5\4\0\2\5\2\0\3\5" +
            "\13\0\4\4\1\0\1\4\7\0\14\5\3\4\14\0\3\5\1\0" +
            "\11\4\1\0\3\4\1\0\26\4\1\0\7\4\1\0\2\4\1\0" +
            "\5\4\2\0\1\5\1\4\10\5\1\0\3\5\1\0\3\5\2\0" +
            "\1\4\17\0\2\4\2\5\2\0\12\5\1\0\1\4\17\0\3\5" +
            "\1\0\10\4\2\0\2\4\2\0\26\4\1\0\7\4\1\0\2\4" +
            "\1\0\5\4\2\0\1\5\1\4\6\5\3\0\2\5\2\0\3\5" +
            "\10\0\2\5\4\0\2\4\1\0\3\4\4\0\12\5\1\0\1\4" +
            "\20\0\1\5\1\4\1\0\6\4\3\0\3\4\1\0\4\4\3\0" +
            "\2\4\1\0\1\4\1\0\2\4\3\0\2\4\3\0\3\4\3\0" +
            "\10\4\1\0\3\4\4\0\5\5\3\0\3\5\1\0\4\5\11\0" +
            "\1\5\17\0\11\5\11\0\1\4\7\0\3\5\1\0\10\4\1\0" +
            "\3\4\1\0\27\4\1\0\12\4\1\0\5\4\4\0\7\5\1\0" +
            "\3\5\1\0\4\5\7\0\2\5\11\0\2\4\4\0\12\5\22\0" +
            "\2\5\1\0\10\4\1\0\3\4\1\0\27\4\1\0\12\4\1\0" +
            "\5\4\2\0\1\5\1\4\7\5\1\0\3\5\1\0\4\5\7\0" +
            "\2\5\7\0\1\4\1\0\2\4\4\0\12\5\22\0\2\5\1\0" +
            "\10\4\1\0\3\4\1\0\27\4\1\0\20\4\4\0\6\5\2\0" +
            "\3\5\1\0\4\5\11\0\1\5\10\0\2\4\4\0\12\5\22\0" +
            "\2\5\1\0\22\4\3\0\30\4\1\0\11\4\1\0\1\4\2\0" +
            "\7\4\3\0\1\5\4\0\6\5\1\0\1\5\1\0\10\5\22\0" +
            "\2\5\15\0\60\4\1\5\2\4\7\5\4\0\10\4\10\5\1\0" +
            "\12\5\47\0\2\4\1\0\1\4\2\0\2\4\1\0\1\4\2\0" +
            "\1\4\6\0\4\4\1\0\7\4\1\0\3\4\1\0\1\4\1\0" +
            "\1\4\2\0\2\4\1\0\4\4\1\5\2\4\6\5\1\0\2\5" +
            "\1\4\2\0\5\4\1\0\1\4\1\0\6\5\2\0\12\5\2\0" +
            "\2\4\42\0\1\4\27\0\2\5\6\0\12\5\13\0\1\5\1\0" +
            "\1\5\1\0\1\5\4\0\2\5\10\4\1\0\42\4\6\0\24\5" +
            "\1\0\2\5\4\4\4\0\10\5\1\0\44\5\11\0\1\5\71\0" +
            "\42\4\1\0\5\4\1\0\2\4\1\0\7\5\3\0\4\5\6\0" +
            "\12\5\6\0\6\4\4\5\106\0\46\4\12\0\51\4\7\0\132\4" +
            "\5\0\104\4\5\0\122\4\6\0\7\4\1\0\77\4\1\0\1\4" +
            "\1\0\4\4\2\0\7\4\1\0\1\4\1\0\4\4\2\0\47\4" +
            "\1\0\1\4\1\0\4\4\2\0\37\4\1\0\1\4\1\0\4\4" +
            "\2\0\7\4\1\0\1\4\1\0\4\4\2\0\7\4\1\0\7\4" +
            "\1\0\27\4\1\0\37\4\1\0\1\4\1\0\4\4\2\0\7\4" +
            "\1\0\47\4\1\0\23\4\16\0\11\5\56\0\125\4\14\0\u026c\4" +
            "\2\0\10\4\12\0\32\4\5\0\113\4\3\0\3\4\17\0\15\4" +
            "\1\0\4\4\3\5\13\0\22\4\3\5\13\0\22\4\2\5\14\0" +
            "\15\4\1\0\3\4\1\0\2\5\14\0\64\4\40\5\3\0\1\4" +
            "\3\0\2\4\1\5\2\0\12\5\41\0\3\5\2\0\12\5\6\0" +
            "\130\4\10\0\51\4\1\5\126\0\35\4\3\0\14\5\4\0\14\5" +
            "\12\0\12\5\36\4\2\0\5\4\u038b\0\154\4\224\0\234\4\4\0" +
            "\132\4\6\0\26\4\2\0\6\4\2\0\46\4\2\0\6\4\2\0" +
            "\10\4\1\0\1\4\1\0\1\4\1\0\1\4\1\0\37\4\2\0" +
            "\65\4\1\0\7\4\1\0\1\4\3\0\3\4\1\0\7\4\3\0" +
            "\4\4\2\0\6\4\4\0\15\4\5\0\3\4\1\0\7\4\17\0" +
            "\4\5\32\0\5\5\20\0\2\4\23\0\1\4\13\0\4\5\6\0" +
            "\6\5\1\0\1\4\15\0\1\4\40\0\22\4\36\0\15\5\4\0" +
            "\1\5\3\0\6\5\27\0\1\4\4\0\1\4\2\0\12\4\1\0" +
            "\1\4\3\0\5\4\6\0\1\4\1\0\1\4\1\0\1\4\1\0" +
            "\4\4\1\0\3\4\1\0\7\4\3\0\3\4\5\0\5\4\26\0" +
            "\44\4\u0e81\0\3\4\31\0\11\4\6\5\1\0\5\4\2\0\5\4" +
            "\4\0\126\4\2\0\2\5\2\0\3\4\1\0\137\4\5\0\50\4" +
            "\4\0\136\4\21\0\30\4\70\0\20\4\u0200\0\u19b6\4\112\0\u51a6\4" +
            "\132\0\u048d\4\u0773\0\u2ba4\4\u215c\0\u012e\4\2\0\73\4\225\0\7\4" +
            "\14\0\5\4\5\0\1\4\1\5\12\4\1\0\15\4\1\0\5\4" +
            "\1\0\1\4\1\0\2\4\1\0\2\4\1\0\154\4\41\0\u016b\4" +
            "\22\0\100\4\2\0\66\4\50\0\15\4\3\0\20\5\20\0\4\5" +
            "\17\0\2\4\30\0\3\4\31\0\1\4\6\0\5\4\1\0\207\4" +
            "\2\0\1\5\4\0\1\4\13\0\12\5\7\0\32\4\4\0\1\4" +
            "\1\0\32\4\12\0\132\4\3\0\6\4\2\0\6\4\2\0\6\4" +
            "\2\0\3\4\3\0\2\4\3\0\2\4\22\0\3\5\4\0";

    /**
     * Translates characters to character classes
     */
    private static final char[] ZZ_CMAP = zzUnpackCMap(ZZ_CMAP_PACKED);

    /**
     * Translates DFA states to action switch labels.
     */
    private static final int[] ZZ_ACTION = zzUnpackAction();

    private static final String ZZ_ACTION_PACKED_0 =
        "\1\0\1\1\1\2\1\3\1\4\1\1\1\2\1\1" +
            "\1\5\1\6\3\3\1\7\1\10\1\11\1\12\1\13" +
            "\1\14\1\15\2\2\1\16\1\17\1\2\2\5\2\6" +
            "\3\3\1\2\1\17\2\5\2\6\3\3\1\0\1\20" +
            "\1\3\1\21\1\17\1\22";

    private static int[] zzUnpackAction() {
        int[] result = new int[47];
        int offset = 0;
        offset = zzUnpackAction(ZZ_ACTION_PACKED_0, offset, result);
        return result;
    }

    private static int zzUnpackAction(String packed, int offset, int[] result) {
        int i = 0;       /* index in packed string  */
        int j = offset;  /* index in unpacked array */
        int l = packed.length();
        while (i < l) {
            int count = packed.charAt(i++);
            int value = packed.charAt(i++);
            do {
                result[j++] = value;
            }
            while (--count > 0);
        }
        return j;
    }


    /**
     * Translates a state to a row index in the transition table
     */
    private static final int[] ZZ_ROWMAP = zzUnpackRowMap();

    private static final String ZZ_ROWMAP_PACKED_0 =
        "\0\0\0\42\0\104\0\146\0\210\0\252\0\314\0\356" +
            "\0\u0110\0\u0132\0\u0154\0\u0176\0\u0198\0\42\0\42\0\42" +
            "\0\42\0\42\0\42\0\42\0\u01ba\0\u01dc\0\u01fe\0\u0220" +
            "\0\u0242\0\42\0\u0264\0\u0286\0\42\0\u02a8\0\u02ca\0\u02ec" +
            "\0\u030e\0\u0330\0\u0352\0\u0374\0\u0396\0\u03b8\0\u03da\0\u03fc" +
            "\0\u041e\0\u0440\0\146\0\u0462\0\146\0\42\0\146";

    private static int[] zzUnpackRowMap() {
        int[] result = new int[47];
        int offset = 0;
        offset = zzUnpackRowMap(ZZ_ROWMAP_PACKED_0, offset, result);
        return result;
    }

    private static int zzUnpackRowMap(String packed, int offset, int[] result) {
        int i = 0;  /* index in packed string  */
        int j = offset;  /* index in unpacked array */
        int l = packed.length();
        while (i < l) {
            int high = packed.charAt(i++) << 16;
            result[j++] = high | packed.charAt(i++);
        }
        return j;
    }

    /**
     * The transition table of the DFA
     */
    private static final int[] ZZ_TRANS = zzUnpackTrans();

    private static final String ZZ_TRANS_PACKED_0 =
        "\1\2\1\3\1\4\1\5\1\4\1\2\1\6\1\2" +
            "\1\5\1\7\1\4\1\10\1\4\1\2\1\5\1\11" +
            "\1\2\1\12\1\13\3\4\1\14\3\4\1\15\1\16" +
            "\1\17\1\20\1\21\1\22\1\23\1\24\43\0\1\3" +
            "\7\0\1\3\1\0\1\25\1\26\10\0\1\26\15\0" +
            "\2\4\1\0\2\4\3\0\2\4\1\0\1\4\5\0" +
            "\11\4\12\0\1\5\4\0\1\5\5\0\1\5\31\0" +
            "\1\27\1\30\33\0\1\3\7\0\1\3\1\31\1\25" +
            "\1\26\10\0\1\26\15\0\1\25\7\0\1\25\30\0" +
            "\10\11\1\0\5\11\1\0\1\32\1\33\21\11\10\12" +
            "\1\0\5\12\1\0\1\12\1\34\1\35\20\12\1\0" +
            "\2\4\1\0\2\4\3\0\2\4\1\0\1\4\5\0" +
            "\1\4\1\36\7\4\10\0\2\4\1\0\2\4\3\0" +
            "\2\4\1\0\1\4\5\0\5\4\1\37\3\4\10\0" +
            "\2\4\1\0\2\4\3\0\2\4\1\0\1\4\5\0" +
            "\2\4\1\40\6\4\10\0\1\25\7\0\1\25\2\0" +
            "\1\26\10\0\1\26\15\0\1\41\7\0\1\41\3\0" +
            "\1\41\23\0\1\41\10\27\1\0\5\27\1\0\23\27" +
            "\7\42\1\0\32\42\1\0\2\31\6\0\1\31\2\0" +
            "\1\31\10\0\3\31\12\0\3\11\1\43\4\11\1\44" +
            "\31\11\3\12\1\45\4\12\1\46\31\12\1\0\2\4" +
            "\1\0\2\4\3\0\2\4\1\0\1\4\5\0\2\4" +
            "\1\47\6\4\10\0\2\4\1\0\2\4\3\0\2\4" +
            "\1\0\1\4\5\0\6\4\1\50\2\4\10\0\2\4" +
            "\1\0\2\4\3\0\2\4\1\0\1\4\5\0\6\4" +
            "\1\51\2\4\10\0\1\41\7\0\1\41\30\0\7\42" +
            "\1\52\32\42\3\11\1\43\4\11\1\44\6\11\1\32" +
            "\1\33\31\11\1\0\6\11\1\32\1\33\21\11\3\12" +
            "\1\45\4\12\1\46\7\12\1\34\1\35\30\12\1\0" +
            "\7\12\1\34\1\35\20\12\1\0\2\4\1\0\2\4" +
            "\3\0\2\4\1\0\1\4\5\0\3\4\1\53\5\4" +
            "\10\0\2\4\1\0\2\4\3\0\2\4\1\0\1\4" +
            "\5\0\7\4\1\54\1\4\10\0\2\4\1\0\2\4" +
            "\3\0\2\4\1\0\1\4\5\0\6\4\1\55\2\4" +
            "\7\0\6\42\1\56\1\52\32\42\1\0\2\4\1\0" +
            "\2\4\3\0\2\4\1\0\1\4\5\0\3\4\1\57" +
            "\5\4\7\0";

    private static int[] zzUnpackTrans() {
        int[] result = new int[1156];
        int offset = 0;
        offset = zzUnpackTrans(ZZ_TRANS_PACKED_0, offset, result);
        return result;
    }

    private static int zzUnpackTrans(String packed, int offset, int[] result) {
        int i = 0;       /* index in packed string  */
        int j = offset;  /* index in unpacked array */
        int l = packed.length();
        while (i < l) {
            int count = packed.charAt(i++);
            int value = packed.charAt(i++);
            value--;
            do {
                result[j++] = value;
            }
            while (--count > 0);
        }
        return j;
    }


    /* error codes */
    private static final int ZZ_UNKNOWN_ERROR = 0;
    private static final int ZZ_NO_MATCH = 1;
    private static final int ZZ_PUSHBACK_2BIG = 2;
    private static final char[] EMPTY_BUFFER = new char[0];
    private static final int YYEOF = -1;

    /* error messages for the codes above */
    private static final String ZZ_ERROR_MSG[] = {
        "Unkown internal scanner error",
        "Error: could not match input",
        "Error: pushback value was too large"
    };

    /**
     * ZZ_ATTRIBUTE[aState] contains the attributes of state <code>aState</code>
     */
    private static final int[] ZZ_ATTRIBUTE = zzUnpackAttribute();

    private static final String ZZ_ATTRIBUTE_PACKED_0 =
        "\1\0\1\11\13\1\7\11\5\1\1\11\2\1\1\11" +
            "\14\1\1\0\3\1\1\11\1\1";

    private static int[] zzUnpackAttribute() {
        int[] result = new int[47];
        int offset = 0;
        offset = zzUnpackAttribute(ZZ_ATTRIBUTE_PACKED_0, offset, result);
        return result;
    }

    private static int zzUnpackAttribute(String packed, int offset, int[] result) {
        int i = 0;       /* index in packed string  */
        int j = offset;  /* index in unpacked array */
        int l = packed.length();
        while (i < l) {
            int count = packed.charAt(i++);
            int value = packed.charAt(i++);
            do {
                result[j++] = value;
            }
            while (--count > 0);
        }
        return j;
    }

    /** the current state of the DFA */
    private int zzState;

    /** the current lexical state */
    private int zzLexicalState = YYINITIAL;

    /** this buffer contains the current text to be matched and is
        the source of the yytext() string */
    private CharSequence zzBuffer = "";

    /** the textposition at the last accepting state */
    private int zzMarkedPos;

    /** the textposition at the last state to be included in yytext */
    private int zzPushbackPos;

    /** the current text position in the buffer */
    private int zzCurrentPos;

    /** startRead marks the beginning of the yytext() string in the buffer */
    private int zzStartRead;

    /** endRead marks the last character in the buffer, that has been read
        from input */
    private int zzEndRead;

    /**
     * zzAtBOL == true <=> the scanner is currently at the beginning of a line
     */
    private boolean zzAtBOL = true;

    /** zzAtEOF == true <=> the scanner is at the EOF */
    private boolean zzAtEOF;

    private IElementType myTokenType;
    private int myState;

    /** denotes if the user-EOF-code has already been executed */
    private boolean zzEOFDone;


    /**
     * Unpacks the compressed character translation table.
     *
     * @param packed the packed character translation table
     * @return the unpacked character translation table
     */
    private static char[] zzUnpackCMap(String packed) {
        char[] map = new char[0x10000];
        int i = 0;  /* index in packed string  */
        int j = 0;  /* index in unpacked array */
        while (i < 1738) {
            int count = packed.charAt(i++);
            char value = packed.charAt(i++);
            do {
                map[j++] = value;
            }
            while (--count > 0);
        }
        return map;
    }

    @Override
    public IElementType getTokenType() {
        if (myTokenType == null) {
           locateToken();
        }
        return myTokenType;
    }

    @Override
    public final int getTokenStart() {
        if (myTokenType == null) {
            locateToken();
        }
        return zzStartRead;
    }

    @Override
    public final int getTokenEnd() {
        if (myTokenType == null) {
            locateToken();
        }
        return getTokenStart() + yylength();
    }

    @Override
    public void advance() {
        if (myTokenType == null) {
            locateToken();
        }
        myTokenType = null;
    }

    @Override
    public int getState() {
        if (myTokenType == null) {
          locateToken();
        }
        return myState;
    }

    @Override
    public void start(final CharSequence buffer, int startOffset, int endOffset, final int initialState) {
        reset(buffer, startOffset, endOffset, initialState);
        myTokenType = null;
    }

    @Override
    public CharSequence getBufferSequence() {
        return zzBuffer;
    }

    @Override
    public int getBufferEnd() {
        return zzEndRead;
    }

    public void reset(CharSequence buffer, int start, int end, int initialState) {
        zzBuffer = buffer;
        zzCurrentPos = zzMarkedPos = zzStartRead = start;
        zzPushbackPos = 0;
        zzAtEOF = false;
        zzAtBOL = true;
        zzEndRead = end;
        yybegin(initialState);
        myTokenType = null;
    }

    private void locateToken() {
        if (myTokenType != null) {
            return;
        }
        try {
            myState = yystate();
            myTokenType = advanceImpl();
        }
        catch (java.io.IOException e) { /*Can't happen*/ }
        catch (Error e) {
            // add lexer class name to the error
            final Error error = new Error(getClass().getName() + ": " + e.getMessage());
            error.setStackTrace(e.getStackTrace());
            throw error;
        }
    }

    /**
     * Refills the input buffer.
     *
     * @return <code>false</code>, iff there was new input.
     * @throws java.io.IOException if any I/O-Error occurs
     */
    private boolean zzRefill() throws java.io.IOException {
        return true;
    }


    /**
     * Returns the current lexical state.
     */
    public final int yystate() {
        return zzLexicalState;
    }


    /**
     * Enters a new lexical state
     *
     * @param newState the new lexical state
     */
    public final void yybegin(int newState) {
        zzLexicalState = newState;
    }


    /**
     * Returns the text matched by the current regular expression.
     */
    public final CharSequence yytext() {
        return zzBuffer.subSequence(zzStartRead, zzMarkedPos);
    }


    /**
     * Returns the character at position <tt>pos</tt> from the
     * matched text.
     *
     * It is equivalent to yytext().charAt(pos), but faster
     *
     * @param pos the position of the character to fetch.
     *            A value from 0 to yylength()-1.
     *
     * @return the character at position pos
     */
    public final char yycharat(int pos) {
        return zzBuffer.charAt(zzStartRead + pos);
    }


    /**
     * Returns the length of the matched text region.
     */
    public final int yylength() {
        return zzMarkedPos - zzStartRead;
    }


    /**
     * Reports an error that occured while scanning.
     *
     * In a wellformed scanner (no or only correct usage of
     * yypushback(int) and a match-all fallback rule) this method
     * will only be called with things that "Can't Possibly Happen".
     * If this method is called, something is seriously wrong
     * (e.g. a JFlex bug producing a faulty scanner etc.).
     *
     * Usual syntax/scanner level error handling should be done
     * in error fallback rules.
     *
     * @param errorCode the code of the errormessage to display
     */
    private void zzScanError(int errorCode) {
        String message;
        try {
            message = ZZ_ERROR_MSG[errorCode];
        }
        catch (ArrayIndexOutOfBoundsException e) {
            message = ZZ_ERROR_MSG[ZZ_UNKNOWN_ERROR];
        }

        throw new Error(message);
    }


    /**
     * Pushes the specified amount of characters back into the input stream.
     *
     * They will be read again by then next call of the scanning method
     *
     * @param number the number of characters to be read again.
     *               This number must not be greater than yylength()!
     */
    public void yypushback(int number) {
        if (number > yylength()) {
          zzScanError(ZZ_PUSHBACK_2BIG);
        }

        zzMarkedPos -= number;
    }


    /**
     * Contains user EOF-code, which will be executed exactly once,
     * when the end of file is reached
     */
    private void zzDoEOF() {
        if (!zzEOFDone) {
            zzEOFDone = true;

        }
    }


    /**
     * Resumes scanning until the next regular expression is matched,
     * the end of input is encountered or an I/O-Error occurs.
     *
     * @return the next token
     * @throws java.io.IOException if any I/O-Error occurs
     */
    public IElementType advanceImpl() throws java.io.IOException {
        int zzInput;
        int zzAction;

        // cached fields:
        int zzCurrentPosL;
        int zzMarkedPosL;
        int zzEndReadL = zzEndRead;
        CharSequence zzBufferL = zzBuffer;
        char[] zzCMapL = ZZ_CMAP;

        int[] zzTransL = ZZ_TRANS;
        int[] zzRowMapL = ZZ_ROWMAP;
        int[] zzAttrL = ZZ_ATTRIBUTE;

        while (true) {
            zzMarkedPosL = zzMarkedPos;

            zzAction = -1;

            zzCurrentPosL = zzCurrentPos = zzStartRead = zzMarkedPosL;

            zzState = ZZ_LEXSTATE[zzLexicalState];


            zzForAction: {
                while (true) {
                    if (zzCurrentPosL < zzEndReadL) {
                        zzInput = zzBufferL.charAt(zzCurrentPosL++);
                    }
                    else if (zzAtEOF) {
                        zzInput = YYEOF;
                        break zzForAction;
                    }
                    else {
                        // store back cached positions
                        zzCurrentPos = zzCurrentPosL;
                        zzMarkedPos = zzMarkedPosL;
                        boolean eof = zzRefill();
                        // get translated positions and possibly new buffer
                        zzCurrentPosL = zzCurrentPos;
                        zzMarkedPosL = zzMarkedPos;
                        zzBufferL = zzBuffer;
                        zzEndReadL = zzEndRead;
                        if (eof) {
                            zzInput = YYEOF;
                            break zzForAction;
                        }
                        else {
                            zzInput = zzBufferL.charAt(zzCurrentPosL++);
                        }
                    }
                    int zzNext = zzTransL[zzRowMapL[zzState] + zzCMapL[zzInput]];
                    if (zzNext == -1) {
                        break zzForAction;
                    }
                    zzState = zzNext;

                    int zzAttributes = zzAttrL[zzState];
                    if ((zzAttributes & 1) == 1) {
                        zzAction = zzState;
                        zzMarkedPosL = zzCurrentPosL;
                        if ((zzAttributes & 8) == 8) {
                            break zzForAction;
                        }
                    }
                }
            }

            // store back cached position
            zzMarkedPos = zzMarkedPosL;

            switch (zzAction < 0 ? zzAction : ZZ_ACTION[zzAction]) {
                case 13: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.MINUS;
                }
                case 19:
                    break;
                case 7: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.LBRACE;
                }
                case 20:
                    break;
                case 6: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.STRING_LITERAL;
                }
                case 21:
                    break;
                case 9: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.LBRACKET;
                }
                case 22:
                    break;
                case 16: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.TRUE_KEYWORD;
                }
                case 23:
                    break;
                case 12: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.COLON;
                }
                case 24:
                    break;
                case 4: {
                    return JSTokenTypes.WHITE_SPACE;
                }
                case 25:
                    break;
                case 17: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.NULL_KEYWORD;
                }
                case 26:
                    break;
                case 1: {
                    return JSTokenTypes.BAD_CHARACTER;
                }
                case 27:
                    break;
                case 18: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.FALSE_KEYWORD;
                }
                case 28:
                    break;
                case 10: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.RBRACKET;
                }
                case 29:
                    break;
                case 15: {
                    return JSTokenTypes.C_STYLE_COMMENT;
                }
                case 30:
                    break;
                case 3: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.IDENTIFIER;
                }
                case 31:
                    break;
                case 14: {
                    return JSTokenTypes.END_OF_LINE_COMMENT;
                }
                case 32:
                    break;
                case 2: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.NUMERIC_LITERAL;
                }
                case 33:
                    break;
                case 8: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.RBRACE;
                }
                case 34:
                    break;
                case 11: {
                    yybegin(YYINITIAL);
                    return JSTokenTypes.COMMA;
                }
                case 35:
                    break;
                case 5: {
                    return JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL;
                }
                case 36:
                    break;
                default:
                    if (zzInput == YYEOF && zzStartRead == zzCurrentPos) {
                        zzAtEOF = true;
                        zzDoEOF();
                        return null;
                    }
                    else {
                        zzScanError(ZZ_NO_MATCH);
                    }
            }
        }
    }
}
