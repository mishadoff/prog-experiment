package com.mishadoff.magic;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.math.BigInteger;
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
 * Demosntrates usage of sun.misc.Unsafe
 * 
 * @author mishadoff
 *
 */
@SuppressWarnings("restriction")
public class UnsafeClient {
    public static void main(String[] args) throws Exception {
        // obtain unsafe instance via reflection
        Unsafe unsafe = getUnsafe();

        // "Info" methods
        info();
        // Avoid constructor
        avoidConstructor();
            
        // sizeof
        System.out.println("B: " + sizeOf(new B()));
        System.out.println("Z: " + sizeOf(new Z()));
        System.out.println("Object: " + sizeOf(new Object()));
        System.out.println("Integer: " + sizeOf(new Integer(0)));   
        System.out.println("Double: " + sizeOf(new Double(0)));
        System.out.println("String: " + sizeOf(new String()));
        System.out.println("BigInteger: " + sizeOf(new BigInteger("0")));
        
        // super array
        superArray();
        
        dataCorruption();
        
        Counter c = //new StupidCounter();
                    //new SyncCounter();
                    //new LockCounter();
                    //new AtomicCounter();
                    new CASCounter();
        int NUM_OF_THREADS = 1000;
        int NUM_OF_INCREMENTS = 100000;
        ExecutorService service = Executors.newFixedThreadPool(NUM_OF_THREADS);
        long before = System.currentTimeMillis();
        for (int i = 0; i < NUM_OF_THREADS; i++) {
            service.submit(new CounterClient(c, NUM_OF_INCREMENTS));
        }
        service.shutdown();
        service.awaitTermination(1, TimeUnit.MINUTES);
        long after = System.currentTimeMillis();
        
        System.out.println("Counter result: " + c.getCounter());
        System.out.println("Time passed in ms: " + (after - before));
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
        Unsafe unsafe = getUnsafe();
        System.out.println("FROM: allocateInstance");
        A o1 = (A) unsafe.allocateInstance(A.class);
            System.out.println(o1.a());
            
        System.out.println("FROM: Constructor");
        A o2 = new A();
            System.out.println(o2.a());
        
        System.out.println("FROM: Reflection");
        A o3 = A.class.newInstance();
            System.out.println(o3.a());
    }

    public static long sizeOf(Object o) {
        Unsafe u = getUnsafe();
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
        
        // get offset
        long maxSize = 0;
        for (Field f : fields) {
            long offset = u.objectFieldOffset(f);
            if (offset > maxSize) {
                maxSize = offset;
            }
        }
        
        return ((maxSize/8) + 1) * 8;   // padding
    }
    
    public static void superArray() {
        long sum = 0;
        // array
        // int[] array = new int[Integer.MAX_VALUE * 2];
        
        long SUPER_SIZE = (long)Integer.MAX_VALUE * 2;
        SuperArray array = new SuperArray(SUPER_SIZE);
        System.out.println("Array size:" + array.size());
        for (int i = 0; i < 100; i++) {
            array.set((long)Integer.MAX_VALUE + i, (byte)3);
            sum += array.get((long)Integer.MAX_VALUE + i);
        }
        System.out.println("Sum of 100 elements:" + sum);
    }
    
    public static void dataCorruption() throws Exception {
        Guard guard = new Guard();
        System.out.println(guard.giveAccess());
        
        // bypass
        // detect name of property
        Unsafe unsafe = getUnsafe();
        Field f = guard.getClass().getDeclaredField("ACCESS_ALLOWED");
        unsafe.putInt(guard, unsafe.objectFieldOffset(f), 42);
        
        System.out.println(guard.giveAccess());
    }
}

/* Big array */
class SuperArray {

    private final static int BYTE = 1;

    private long size;
    private long address;
    
    public SuperArray(long size) {
        this.size = size;
        address = UnsafeClient.getUnsafe().allocateMemory(size * BYTE);
    }
    
    public void set(long i, byte value) {
        UnsafeClient.getUnsafe().putByte(address + i * BYTE, value);
    }
    
    public int get(long idx) {
        return UnsafeClient.getUnsafe().getByte(address + idx * BYTE);
    }
    
    public long size() {
        return size;
    }
}

class Z extends B{
    int z;
}

class A {
    private long a; // not initialized value
    
    public A() {
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
    private int ACCESS_ALLOWED = 0;
    
    public boolean giveAccess() {
        return 42 == ACCESS_ALLOWED;
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
    private WriteLock lock = new ReentrantReadWriteLock().writeLock();
    
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

class CASCounter implements Counter {
    private long counter = 0;
    private Unsafe unsafe;
    private long offset;
    
    public CASCounter() throws Exception {
        unsafe = UnsafeClient.getUnsafe();
        offset = unsafe.objectFieldOffset(CASCounter.class.getDeclaredField("counter"));
    }
    
    @Override
    public void increment() {
        long before = counter;
        while (!unsafe.compareAndSwapLong(this, offset, before, before + 1)) {
            before = counter;
        }
    }
    
    @Override
    public long getCounter() {
        return counter;
    }
}