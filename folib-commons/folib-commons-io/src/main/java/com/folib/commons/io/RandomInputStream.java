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
package com.folib.commons.io;

import java.io.InputStream;
import java.util.Random;

/**
 * @author veadan
 */
public class RandomInputStream
        extends InputStream
{

    private long count;

    private long length;

    private Random random = new Random();


    public RandomInputStream(long length)
    {
        super();
        this.length = length;
    }

    public RandomInputStream(boolean randomSize, long sizeLimit)
    {
        super();
        if (randomSize)
        {
            this.length = getRandomSize(sizeLimit);
        }
    }

    @Override
    public int read()
    {
        if (count >= length)
        {
            return -1;
        }

        count++;

        return random.nextInt();
    }

    public long getRandomSize(long max)
    {
        // nextInt is normally exclusive of the top value,
        // so add 1 to make it inclusive

        return (long)(random.nextDouble() * max);
    }

    public long getCount()
    {
        return count;
    }

    public void setCount(long count)
    {
        this.count = count;
    }

    public long getLength()
    {
        return length;
    }

    public void setLength(long length)
    {
        this.length = length;
    }

}
