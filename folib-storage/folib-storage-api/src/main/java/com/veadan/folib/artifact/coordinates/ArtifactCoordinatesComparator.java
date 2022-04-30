package com.veadan.folib.artifact.coordinates;

import java.util.Comparator;

public class ArtifactCoordinatesComparator<C extends ArtifactCoordinates<C, V>, V extends Comparable<V>>
        implements Comparator<C>
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
