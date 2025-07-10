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
package com.folib.commons.io.diff;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * This is a very simple tool that can give you an idea of where two streams
 * start having a difference. This is nothing fancy and it will stop executing
 * as soon as it has found the first difference. The main purpose of this tool
 * is to provide a way to understand where multiple digest streams start having
 * differences, thus serving as a sort of debug tool.
 *
 * @author veadan
 */
public class BinaryDiff
{

    private static final Logger logger = LoggerFactory.getLogger(BinaryDiff.class);

    private byte[] bytes1;

    private byte[] bytes2;

    private long numberOfBytesToShowUponDifference = 100;


    public BinaryDiff(byte[] bytes1,
                      byte[] bytes2)
    {
        this.bytes1 = bytes1;
        this.bytes2 = bytes2;
    }

    /**
     * Checks for differences between the InputStream-s.
     *
     * @return True, if there are differences; false otherwise.
     */
    public boolean diff()
    {
        try (ByteArrayOutputStream baos1 = new ByteArrayOutputStream();
             ByteArrayOutputStream baos2 = new ByteArrayOutputStream();
             InputStream inputStream1 = new ByteArrayInputStream(bytes1);
             InputStream inputStream2 = new ByteArrayInputStream(bytes2))
        {
            byte[] b1 = new byte[1];
            byte[] b2 = new byte[1];
            int bytesReadFromIS1;
            int bytesReadFromIS2;
            long totalReadFromIS1 = 0L;
            long totalReadFromIS2 = 0L;

            long differencePositionStart;

            // Read byte by byte:
            while ((bytesReadFromIS1 = inputStream1.read(b1)) != -1 &&
                   (bytesReadFromIS2 = inputStream2.read(b2)) != -1)
            {
                totalReadFromIS1 += bytesReadFromIS1;
                totalReadFromIS2 += bytesReadFromIS2;

                if (b1[0] != b2[0])
                {
                    differencePositionStart = totalReadFromIS1;

                    logger.info("Byte at position {} differs: ", differencePositionStart);

                    baos1.write(b1);
                    baos1.flush();

                    baos2.write(b2);
                    baos2.flush();

                    totalReadFromIS1 = readExcerpt(inputStream1, totalReadFromIS1, b1, baos1);
                    totalReadFromIS2 = readExcerpt(inputStream2, totalReadFromIS2, b2, baos2);

                    streamsHaveDifferentLength(totalReadFromIS1, totalReadFromIS2);

                    logger.info("Displaying excerpt of data from input stream 1 after a difference was found: {}",
                                baos1);

                    logger.info("Displaying excerpt of data from input stream 2 after a difference was found: {}",
                                baos2);

                    return true;
                }
            }

            if (streamsHaveDifferentLength(totalReadFromIS1, totalReadFromIS2))
            {
                return true;
            }
        }
        catch (IOException e)
        {
            logger.error(e.getMessage(), e);
        }

        return false;
    }

    private long readExcerpt(InputStream is,
                             long totalReadFromIS,
                             byte[] b,
                             ByteArrayOutputStream baos)
            throws IOException
    {
        int bytesReadFromIS;
        long bytesShown1 = 0L;
        while ((bytesReadFromIS = is.read(b)) != -1 && bytesShown1 < numberOfBytesToShowUponDifference)
        {
            totalReadFromIS += bytesReadFromIS;

            baos.write(b);
            baos.flush();

            bytesShown1++;
        }

        return totalReadFromIS;
    }

    private boolean streamsHaveDifferentLength(long totalReadFromIS1,
                                               long totalReadFromIS2)
    {
        if (totalReadFromIS1 != totalReadFromIS2)
        {
            if (totalReadFromIS1 == -1)
            {
                logger.info("Input stream 1 ended before input stream 2 at byte {}", totalReadFromIS1);
            }

            if (totalReadFromIS2 == -1)
            {
                logger.info("Input stream 2 ended before input stream 1 at byte {}", totalReadFromIS2);
            }

            return true;
        }

        return false;
    }

    public byte[] getBytes1()
    {
        return bytes1;
    }

    public void setBytes1(byte[] bytes1)
    {
        this.bytes1 = bytes1;
    }

    public byte[] getBytes2()
    {
        return bytes2;
    }

    public void setBytes2(byte[] bytes2)
    {
        this.bytes2 = bytes2;
    }

    public long getNumberOfBytesToShowUponDifference()
    {
        return numberOfBytesToShowUponDifference;
    }

    public void setNumberOfBytesToShowUponDifference(long numberOfBytesToShowUponDifference)
    {
        this.numberOfBytesToShowUponDifference = numberOfBytesToShowUponDifference;
    }

}
