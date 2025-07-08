package com.folib.promotion;

import org.apache.tinkerpop.shaded.kryo.Kryo;
import org.apache.tinkerpop.shaded.kryo.io.Input;
import org.apache.tinkerpop.shaded.kryo.io.Output;
import org.apache.tinkerpop.shaded.objenesis.strategy.StdInstantiatorStrategy;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

/**
 * @author veadan
 * @date 2024/2/13 14:41
 */
public class KryoSerializationUtil {
    // ThreadLocal 为每个线程提供独立的 Kryo 实例，确保线程安全
    private static final ThreadLocal<Kryo> kryoThreadLocal = ThreadLocal.withInitial(() -> {
        Kryo kryo = new Kryo();
        kryo.setInstantiatorStrategy(new Kryo.DefaultInstantiatorStrategy(new StdInstantiatorStrategy()));
        // 配置 Kryo 实例
        // kryo.setReferences(true); // 默认为 true
        // kryo.setRegistrationRequired(false); // 默认为 false
        return kryo;
    });

    /**
     * 序列化方法：将对象序列化为字节数组
     *
     * @param object 待序列化的对象
     * @return 序列化后的字节数组
     */
    public static byte[] serialize(Object object) {
        Kryo kryo = kryoThreadLocal.get();
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
        Output output = new Output(byteArrayOutputStream);
        kryo.writeClassAndObject(output, object);
        output.close();
        return byteArrayOutputStream.toByteArray();
    }

    /**
     * 反序列化方法：将字节数组反序列化为原始对象
     *
     * @param byteArray 待反序列化的字节数组
     * @return 反序列化后的对象
     */
    public static <T> T deserialize(byte[] byteArray) {
        Kryo kryo = kryoThreadLocal.get();
        ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(byteArray);
        Input input = new Input(byteArrayInputStream);
        T object = (T) kryo.readClassAndObject(input);
        input.close();
        return object;
    }
}
