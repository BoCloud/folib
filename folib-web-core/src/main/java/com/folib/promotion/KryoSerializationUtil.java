/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
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
