package com.veadan.folib.storage.metadata.maven.comparators;

import java.util.Comparator;

import org.apache.maven.artifact.repository.metadata.SnapshotVersion;
import org.apache.maven.artifact.versioning.ComparableVersion;

/**
 * @author Steve Todorov <s.todorov@itnews-bg.com>
 */
public class SnapshotVersionComparator
        implements Comparator<SnapshotVersion>
{

    public int compare(SnapshotVersion v1, SnapshotVersion v2)
    {
        if (v1 == null || v2 == null)
        {
            throw new IllegalArgumentException();
        }

        ComparableVersion av1 = new ComparableVersion(v1.getVersion());
        ComparableVersion av2 = new ComparableVersion(v2.getVersion());

        return av1.compareTo(av2);
    }

}
