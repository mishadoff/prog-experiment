package com.mishadoff.magic;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.instrument.Instrumentation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.util.Date;
import java.util.HashSet;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock.WriteLock;

import sun.misc.Unsafe;

/**
 * Demonstrates usage of sun.misc.Unsafe
 * 
 * @author mishadoff
 *
 */
@SuppressWarnings({"restriction", "unused"})
public class UnsafeClient {
    
    public static void main(String[] args) throws Exception {
        // obtain unsafe instance via reflection
        
        // reflection
        Unsafe unsafe = getUnsafe();
        
        // bootclasspath
        // java -Xbootclasspath:/home/mishadoff/soft/jdk1.7.0/jre/lib/rt.jar:/home/mishadoff/soft/jdk1.7.0/lib/tools.jar:. com.mishadoff.magic.UnsafeClient
        // Unsafe unsafe2 = Unsafe.getUnsafe();
        
        // "Info" methods
        info();
        // Avoid constructor
        avoidConstructor();
        
        // allocateInstanceTest
        allocateInstanceTest();
            
        // sizeof
        System.out.println("B: " + sizeOf(new B()) + " = " + sizeOf2(new B()));
        System.out.println("Z: " + sizeOf(new Z()) + " = " + sizeOf2(new Z()));
        System.out.println("Z2: " + sizeOf(new Z2()) + " = " + sizeOf2(new Z2()));
        System.out.println("Object: " + sizeOf(new Object()) + " = " + sizeOf2(new Object()));
        System.out.println("Integer: " + sizeOf(new Integer(0)) + " = " + sizeOf2(new Integer(0)));
        System.out.println("Long: " + sizeOf(new Long(0L)) + " = " + sizeOf2(new Long(0L)));  
        System.out.println("Double: " + sizeOf(new Double(0)) + " = " + sizeOf2(new Double(0)));
        System.out.println("String: " + sizeOf(new String()) + " = " + sizeOf2(new String()));
        System.out.println("Date: " + sizeOf(new Date()) + " = " + sizeOf2(new Date()));
        System.out.println("BigInteger: " + sizeOf(new BigInteger("0")) + " = " + sizeOf2(new BigInteger("0")));
        
        Thread.sleep(1000);
        
        B toCopy = new B();
        toCopy.a = 10;
        B b = (B) shallowCopy(toCopy);
        toCopy.a = 666;
        System.out.println(b.a);
        
        //str
        
        String s1 = "Hell O'World";
        String s2 = (String) shallowCopy(s1);
        
        dynamicClasses();
        
        veryUncheckedCast();

        hidePassword();
        
        // super array
        //superArray();
        
        defineTrueFalse();
        //defineTrueFalseReflection();
        
        dataCorruption();
        
        
        // throw checked exception
        // throwCheckedException1();
        // throwCheckedException2();
        
        
        Counter counter = new StupidCounter();
                    //new SyncCounter();
                    //new LockCounter();
                    //new AtomicCounter();
                   //new CASCounter();
        int NUM_OF_THREADS = 1000;
        int NUM_OF_INCREMENTS = 100000;
        ExecutorService service = Executors.newFixedThreadPool(NUM_OF_THREADS);
        long before = System.currentTimeMillis();
        for (int i = 0; i < NUM_OF_THREADS; i++) {
            service.submit(new CounterClient(counter, NUM_OF_INCREMENTS));
        }
        service.shutdown();
        service.awaitTermination(1, TimeUnit.MINUTES);
        long after = System.currentTimeMillis();
        
        System.out.println("Counter result: " + counter.getCounter());
        System.out.println("Time passed in ms: " + (after - before));
        
        // 
        
        extendFinalClass();
    }
        
    public static Unsafe getUnsafe() {
        try {
            Field f = Unsafe.class.getDeclaredField("theUnsafe");
            f.setAccessible(true);
            return (Unsafe) f.get(null);
        } catch (Exception e) {
            throw new RuntimeException("Can not obtain unsafe object");
        }
    }
    
    public static void info() {
        Unsafe unsafe = getUnsafe();
        // returns size of native pointer in bytes: 4 for 32bit, 8 for 64bit
        System.out.println("Adress size: " + unsafe.addressSize());
        // returns size of memory page in bytes
        System.out.println("Page size: " + unsafe.pageSize());
    }
    
    public static void avoidConstructor() throws Exception {
        A1 o1 = new A1();
        o1.a(); // 1
        
        A1 o2 = A1.class.newInstance();
        o2.a(); // 1
            
        A1 o3 = (A1) getUnsafe().allocateInstance(A1.class);
        o3.a(); // 0
    }
    
    @SuppressWarnings("unchecked")
    public static void dynamicClasses() throws Exception {
        
        byte[] classContents = getClassContent();
        Class c = getUnsafe().defineClass(
                null, classContents, 0, classContents.length);
        c.getMethod("a").invoke(c.newInstance(), null); // 1
        
    }

    static void veryUncheckedCast() {
        
        Integer num = new Integer(666);
        //print((String) (Object) num); // ClassCastException
        
        long intClassAddress = normalize(getUnsafe().getInt(0, 4L));
        long strClassAddress = normalize(getUnsafe().getInt("", 4L));
        getUnsafe().putAddress(intClassAddress + 36, strClassAddress);
        
        print((String) (Object) num); // 667
        
    }
    
    static void hidePassword() throws Exception {
        
        String password = new String("l00k@myHor$e");
        String fake = new String(password.replaceAll(".", "?"));
        System.out.println(password); // l00k@myHor$e
        System.out.println(fake); // ????????????
        
        getUnsafe().copyMemory(
                fake, 0L, null, toAddress(password), sizeOf(password));
        
        System.out.println(password); // ????????????
        System.out.println(fake); // ????????????
    }
    
    static void print(String message) {
        Integer i = (Integer) (Object) message;
        System.out.println(i + 1);
    }
    
    private static byte[] getClassContent() throws Exception {
        File f = new File("/home/mishadoff/tmp/A.class");
        FileInputStream input = new FileInputStream(f);
        byte[] content = new byte[(int)f.length()];
        input.read(content);
        input.close();
        return content;
    }
    
    private static void extendFinalClass() {

        long myStrAddress = normalize(getUnsafe().getInt(new StringExt(""), 4L));
        long strClassAddress = normalize(getUnsafe().getInt("", 4L));
        getUnsafe().putInt(myStrAddress + 16, 0x24); // super_count
        getUnsafe().putInt(strClassAddress + 80, 0x1); // final
        getUnsafe().putAddress(myStrAddress + 36, strClassAddress);
        
        // standard api
        new StringExt("javaj").isPalindrom(); // true
        new StringExt("scalaz").isPalindrom(); // false
        ((String) (Object) new StringExt("top")).length(); // 3
        String toPrint = 
                ((String) (Object) new StringExt("dart")) +
                ((String) (Object) new StringExt("vader"));
        System.out.println(toPrint);
    }
    
    private static void throwCheckedException1() throws IOException {
        throw new IOException();
    }
    
    private static void throwCheckedException2() {
        getUnsafe().throwException(new IOException());
    }
   
    
    
    class TimeSeries {
        long number;
        double duration;
    }
    
    interface Output {
        void writeLong(long lo);
        void writeDouble(double d);
    }
    
    void writeObject(Output out, Object obj) {
        out.writeLong(getUnsafe().getLong(obj, 4L));
        out.writeDouble(getUnsafe().getDouble(obj, 12L));
    }
    
    interface Input {
        long readLong();
        double readDouble();
    }
    
    Object readObject(Input in, Class cl) throws Exception {
        Object empty = getUnsafe().allocateInstance(cl);
        getUnsafe().putLong(empty, 4L, in.readLong());
        getUnsafe().putDouble(empty, 12L, in.readDouble());
        return empty;
    }

    @SuppressWarnings("rawtypes")
    public static long sizeOf2(Object o) {
        HashSet<Field> fields = new HashSet<Field>();
        Class c = o.getClass();
        while (c != Object.class) {
            for (Field f : c.getDeclaredFields()) {
                if ((f.getModifiers() & Modifier.STATIC) == 0) {
                    fields.add(f);
                }
            }
            c = c.getSuperclass();
        }
        // get maximum offset
        long maxSize = 0; 
        for (Field f : fields) {
            long offset = getUnsafe().objectFieldOffset(f);
            if (offset > maxSize) {
                maxSize = offset;
            }
        }
        
        return ((maxSize/8) + 1) * 8;   // padding
    }
    
    public static long sizeOf(Object object){
        return getUnsafe().getAddress(
                 normalize(getUnsafe().getInt(object, 4L)) + 12L);
    }
    
    private static long normalize(int value) {
        if(value >= 0) return value;
        return (~0L >>> 32) & value;
    }
    
    static Object shallowCopy(Object obj) {
        long size = sizeOf(obj);
        long start = toAddress(obj);
        long address = getUnsafe().allocateMemory(size);
        getUnsafe().copyMemory(start, address, size);
        return fromAddress(address);
    }
    
    static long toAddress(Object obj) {
        Object[] array = new Object[] {obj};
        long baseOffset = getUnsafe().arrayBaseOffset(Object[].class);
        return normalize(getUnsafe().getInt(array, baseOffset));
    }
    
    static Object fromAddress(long address) {
        Object[] array = new Object[] {null};
        long baseOffset = getUnsafe().arrayBaseOffset(Object[].class);
        getUnsafe().putLong(array, baseOffset, address);
        return array[0];
    }

    public static void defineTrueFalseReflection() throws Exception {
        System.out.println("Define TRUE FALSE");
        Field field = Boolean.class.getField("TRUE");
        field.setAccessible(true);
        Field modifiersField = Field.class.getDeclaredField("modifiers");
        modifiersField.setAccessible(true);
        modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
        field.set(null, false);
    }
    
    public static void defineTrueFalse() throws Exception {
        // #define TRUE FALSE
        System.out.println("Corrupt boolean");
        
        System.out.println("Usual behaviour");
        if (Boolean.TRUE) {
            System.out.println("TRUE");
        } else {
            System.out.println("FALSE");
        }
        
        // corrupt
        Field trueField = Boolean.class.getField("TRUE");
        
        getUnsafe().putObject(
                getUnsafe().staticFieldBase(trueField),
                getUnsafe().staticFieldOffset(trueField), 
                false);
        
        System.out.format("true is so %s\n", true);
        if (Boolean.TRUE) {
            System.out.println("TRUE");
        } else {
            System.out.println("FALSE");
        }
        
        
    }

    static B b = new B();
    static B copy = new B();
    
    public static void strcpyTrick() throws Exception {
        getUnsafe().copyMemory(b, 0, copy, 0, 32);
        System.out.println("A is " + copy.a);
    }
    
    public static void superArray() {
        long sum = 0;
        // array
        // int[] array = new int[Integer.MAX_VALUE * 2];
        
        long SUPER_SIZE = (long)Integer.MAX_VALUE * 2;
        SuperArray array = new UnsafeClient().new SuperArray(SUPER_SIZE);
        System.out.println("Array size:" + array.size());
        for (int i = 0; i < 100; i++) {
            array.set((long)Integer.MAX_VALUE + i, (byte)3);
            sum += array.get((long)Integer.MAX_VALUE + i);
        }
        System.out.println("Sum of 100 elements:" + sum);
    }
    
    public static void dataCorruption() throws Exception {
        System.out.println("Data Corruption");
        
        Guard guard = new Guard();
        Guard neighbour = new Guard();
        
        guard.giveAccess(); // false
        neighbour.giveAccess(); // false
        // bypass
        Field f = guard.getClass().getDeclaredField("ACCESS_ALLOWED");
        long offset = getUnsafe().objectFieldOffset(f);
        getUnsafe().putInt(guard, offset, 42);
        getUnsafe().putInt(guard, 16 + offset, 42);
        
        guard.giveAccess(); // true
        neighbour.giveAccess(); // true
        
    }
    
    static void allocateInstanceTest() throws Exception {
        long sum = 0; long COUNT = 1_000_000_000; long before, after;
        
        before = System.currentTimeMillis();
        for (int i = 0; i < COUNT; i++) {
            sum += new Integer(0);
        }
        after = System.currentTimeMillis() - before;
        System.out.println("Constructor: " + after);
        
        sum = 0;
        Unsafe unsafe = getUnsafe();
        before = System.currentTimeMillis();
        for (int i = 0; i < COUNT; i++) {
            sum += (int) unsafe.allocateInstance(Integer.class);
        }
        after = System.currentTimeMillis() - before;
        System.out.println("AllocateInstance: " + after);
        Thread.sleep(2000);
    }
    
    class SuperArray {
        private final static int BYTE = 1;

        private long size;
        private long address;
        
        public SuperArray(long size) {
            this.size = size;
            address = getUnsafe().allocateMemory(size * BYTE);
        }
        
        public void set(long idx, byte value) {
            getUnsafe().putByte(address + idx * BYTE, value);
        }
        
        public int get(long idx) {
            return getUnsafe().getByte(address + idx * BYTE);
        }
        
        public long size() { return size; }
    }
}

class Z extends B{
    int z;
}

class Z2 extends B{
    byte z;
}

class A1 {
    private long a = 2; // not initialized value, 0
    
    public A1() {
        this.a = 1; // initialization
    }

    public long a() { return this.a; }
}

class B {
    int a;
    long b;
    char c;
}

class C {
    static int a;
    static long b;
}

class Guard {
    private int ACCESS_ALLOWED = 1;
    
    public boolean giveAccess() {
        return 42 == ACCESS_ALLOWED;
    }
    
    public int getAllowedCode() {
        return ACCESS_ALLOWED;
    }
}

interface Counter {
    void increment();
    long getCounter();
}

class CounterClient implements Runnable {
    private Counter c;
    private int num;
    
    public CounterClient(Counter c, int num) {
        this.c = c;
        this.num = num;
    }

    @Override
    public void run() {
        for (int i = 0; i < num; i++) {
            c.increment();
        }
    }
}

class StupidCounter implements Counter {
    private long counter = 0;
    
    @Override
    public void increment() {
        counter++;
    }
    
    @Override
    public long getCounter() {
        return counter;
    }
}

class SyncCounter implements Counter {
    private long counter = 0;
    
    @Override
    public synchronized void increment() {
        counter++;
    }
    
    @Override
    public long getCounter() {
        return counter;
    }
}

class LockCounter implements Counter {
    private long counter = 0;
    private WriteLock lock = 
        new ReentrantReadWriteLock().writeLock();
    
    @Override
    public void increment() {
        lock.lock();
        counter++;
        lock.unlock();
    }
    
    @Override
    public long getCounter() {
        return counter;
    }
}

class AtomicCounter implements Counter {
    AtomicLong counter = new AtomicLong(0);
    
    @Override
    public void increment() {
        counter.incrementAndGet();
    }
    
    @Override
    public long getCounter() {
        return counter.get();
    }
}

@SuppressWarnings("restriction")
class CASCounter implements Counter {
    private long counter = 0;
    private Unsafe unsafe;
    private long offset;
    
    public CASCounter() throws Exception {
        unsafe = UnsafeClient.getUnsafe();
        offset = unsafe.objectFieldOffset(
                CASCounter.class.getDeclaredField("counter"));
    }
    
    @Override
    public void increment() {
        long before = counter;
        while (!unsafe.compareAndSwapLong(this, offset, before, before + 1)) {
            before = counter;
        }
    }
    
    @Override
    public long getCounter() { return counter; }
}