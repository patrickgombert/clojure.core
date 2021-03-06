package clojure.lang.platform.numbers;

public final class BitOps {

  public static long numberBitNot(Object x) {
    long lx = Coercion.toBitOperand(x);
    return ~lx;
  }

  public static long numberBitAnd(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx & ly;
  }

  public static long numberBitAndNot(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx & ~ly;
  }

  public static int integerPreserveBitAnd(int x, int y) {
    return x & y;
  }

  public static long numberBitOr(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx | ly;
  }

  public static int integerPreserveBitOr(int x, int y) {
    return x | y;
  }

  public static long numberBitXor(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx ^ ly;
  }

  public static int integerPreserveBitXor(int x, int y) {
    return x ^ y;
  }

  public static long numberBitClear(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx & ~(1L << ly);
  }

  public static long numberBitSet(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx | (1L << ly);
  }

  public static long numberBitFlip(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx ^ (1L << ly);
  }

  public static boolean numberBitTest(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return (lx & (1L << ly)) != 0;
  }

  public static long numberBitShiftLeft(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx << ly;
  }

  public static int integerPreserveBitShiftLeft(int x, int y) {
    return x << y;
  }

  public static long numberBitShiftRight(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx >> ly;
  }

  public static long numberUnsignedBitShiftRight(Object x, Object y) {
    long lx = Coercion.toBitOperand(x);
    long ly = Coercion.toBitOperand(y);
    return lx >>> ly;
  }

  public static int integerPreserveUnsignedBitShiftRight(int x, int y) {
    return x >>> y;
  }

}

