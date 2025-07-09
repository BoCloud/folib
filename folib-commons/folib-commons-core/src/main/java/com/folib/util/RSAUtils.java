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
package com.folib.util;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.crypto.Cipher;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.security.*;
import java.security.spec.PKCS8EncodedKeySpec;
import java.security.spec.X509EncodedKeySpec;

/**
 * @author veadan
 * @date 2022/11/24
 **/
@Slf4j
@Component
public class RSAUtils {

    @Value("${rsa.publicKey}")
    private String publicKey;

    @Value("${rsa.privateKey}")
    private String privateKey;

    /**
     * RSA最大加密明文大小
     */
    private static final int MAX_ENCRYPT_BLOCK = 117;
    /**
     * RSA最大解密密文大小
     */
    private static final int MAX_DECRYPT_BLOCK = 128;
    /**
     * RSA
     */
    private static final String ALGORITHM_NAME = "RSA";

    /**
     * 生成公钥、私钥
     */
    public void generateKey() {
        try {
            KeyPairGenerator generator = KeyPairGenerator.getInstance(ALGORITHM_NAME);
            generator.initialize(1024);
            KeyPair keyPair = generator.generateKeyPair();
            String privateKey = new String(Base64.encodeBase64(keyPair.getPrivate().getEncoded()));
            System.out.println("privateKey：" + privateKey);
            String publicKey = new String(Base64.encodeBase64(keyPair.getPublic().getEncoded()));
            System.out.println("publicKey：" + publicKey);
        } catch (Exception e) {
            log.error("RSAUtils generateKey error：{}", ExceptionUtils.getStackTrace(e));
        }
    }

    /**
     * RSA加密
     *
     * @param data 待加密数据
     */
    public String encrypt(String data) {
        try {
            PublicKey publicK = getPublicKey(publicKey);
            Cipher cipher = Cipher.getInstance(ALGORITHM_NAME);
            cipher.init(Cipher.ENCRYPT_MODE, publicK);
            int inputLen = data.getBytes().length;
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            int offset = 0;
            byte[] cache;
            int i = 0;
            // 对数据分段加密
            while (inputLen - offset > 0) {
                if (inputLen - offset > MAX_ENCRYPT_BLOCK) {
                    cache = cipher.doFinal(data.getBytes(), offset, MAX_ENCRYPT_BLOCK);
                } else {
                    cache = cipher.doFinal(data.getBytes(), offset, inputLen - offset);
                }
                out.write(cache, 0, cache.length);
                i++;
                offset = i * MAX_ENCRYPT_BLOCK;
            }
            byte[] encryptedData = out.toByteArray();
            out.close();
            // 获取加密内容使用base64进行编码,并以UTF-8为标准转化成字符串
            // 加密后的字符串
            return new String(Base64.encodeBase64(encryptedData));
        } catch (Exception ex) {
            log.error("RSAUtils encrypt error：{}", ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    /**
     * RSA解密
     *
     * @param data 待解密数据
     */
    public String decrypt(String data) {
        try {
            PrivateKey privateK = getPrivateKey(privateKey);
            Cipher cipher = Cipher.getInstance(ALGORITHM_NAME);
            cipher.init(Cipher.DECRYPT_MODE, privateK);
            byte[] dataBytes = Base64.decodeBase64(data);
            int inputLen = dataBytes.length;
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            int offset = 0;
            byte[] cache;
            int i = 0;
            // 对数据分段解密
            while (inputLen - offset > 0) {
                if (inputLen - offset > MAX_DECRYPT_BLOCK) {
                    cache = cipher.doFinal(dataBytes, offset, MAX_DECRYPT_BLOCK);
                } else {
                    cache = cipher.doFinal(dataBytes, offset, inputLen - offset);
                }
                out.write(cache, 0, cache.length);
                i++;
                offset = i * MAX_DECRYPT_BLOCK;
            }
            byte[] decryptedData = out.toByteArray();
            out.close();
            // 解密后的内容
            return new String(decryptedData, StandardCharsets.UTF_8);
        } catch (Exception ex) {
            log.error("RSAUtils decrypt error：{}", ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    /**
     * 获取公钥
     *
     * @param publicKey base64加密的公钥字符串
     */
    public PublicKey getPublicKey(String publicKey) {
        try {
            byte[] decodedKey = Base64.decodeBase64(publicKey.getBytes());
            X509EncodedKeySpec keySpec = new X509EncodedKeySpec(decodedKey);
            KeyFactory keyFactory = KeyFactory.getInstance(ALGORITHM_NAME);
            return keyFactory.generatePublic(keySpec);
        } catch (Exception ex) {
            log.error("RSAUtils getPublicKey error：{}", ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }

    /**
     * 获取私钥
     *
     * @param privateKey base64加密的私钥字符串
     */
    public PrivateKey getPrivateKey(String privateKey) {
        try {
            byte[] decodedKey = Base64.decodeBase64(privateKey.getBytes());
            PKCS8EncodedKeySpec keySpec = new PKCS8EncodedKeySpec(decodedKey);
            KeyFactory keyFactory = KeyFactory.getInstance(ALGORITHM_NAME);
            return keyFactory.generatePrivate(keySpec);
        } catch (Exception ex) {
            log.error("RSAUtils getPrivateKey error：{}", ExceptionUtils.getStackTrace(ex));
        }
        return null;
    }
}

