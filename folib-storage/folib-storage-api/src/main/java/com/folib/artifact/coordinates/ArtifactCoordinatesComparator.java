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
package com.folib.artifact.coordinates;

import java.io.Serializable;
import java.util.Comparator;

public class ArtifactCoordinatesComparator<C extends ArtifactCoordinates<C, V>, V extends Comparable<V>>
        implements Comparator<C>, Serializable
{


    @Override
    public int compare(C o1,
                       C o2)
    {
        if (o1 != null && o2 == null)
        {
            return -1;
        }

        int result = ((result = compareId(o1, o2)) == 0 ? compareVersion(o1, o2) : result);

        return result;
    }

    protected int compareVersion(C o1,
                                 C that)
    {
        V thisNativeVersion = o1.getNativeVersion();
        V thatNativeVersion = that.getNativeVersion();

        if (thisNativeVersion == null && thatNativeVersion == null)
        {
            String thisVersion = o1.getVersion();
            String thatVersion = that.getVersion();

            return compareToken(thisVersion, thatVersion);
        }

        return compareToken(thisNativeVersion, thatNativeVersion);
    }

    protected int compareId(C o1,
                            C that)
    {
        String thisId = o1.getId();
        String thatId = that.getId();

        return compareToken(thisId, thatId);
    }

    protected <T extends Comparable<T>> int compareToken(T thisId,
                                                         T thatId)
    {
        if (thisId == thatId)
        {
            return 0;
        }
        if (thisId == null)
        {
            return Boolean.compare(true, thatId == null);
        }
        return thatId == null ? 1 : Integer.signum(thisId.compareTo(thatId));
    }

}
