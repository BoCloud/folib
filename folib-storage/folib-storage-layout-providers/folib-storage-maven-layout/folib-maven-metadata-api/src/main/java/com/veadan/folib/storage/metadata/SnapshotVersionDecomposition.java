package com.veadan.folib.storage.metadata;

import javax.annotation.Nonnull;
import java.util.regex.Matcher;

import static org.apache.maven.artifact.Artifact.VERSION_FILE_PATTERN;

/**
 * @author Przemyslaw Fusik
 */
public class SnapshotVersionDecomposition
{

    public static final SnapshotVersionDecomposition INVALID = new SnapshotVersionDecomposition(null,
                                                                                                Integer.MIN_VALUE,
                                                                                                null);

    private final String version;
    private final int buildNumber;
    private final String timestamp;

    @Nonnull
    public static SnapshotVersionDecomposition of(final String version)
    {
        if (version == null)
        {
            return INVALID;
        }

        final Matcher matcher = VERSION_FILE_PATTERN.matcher(version);
        if (!matcher.matches())
        {
            return INVALID;
        }

        final int buildNumber = Integer.parseInt(matcher.group(3));
        final String timestamp = matcher.group(2);

        return new SnapshotVersionDecomposition(version, buildNumber, timestamp);
    }

    private SnapshotVersionDecomposition(final String version,
                                         final int buildNumber,
                                         final String timestamp)
    {
        this.version = version;
        this.buildNumber = buildNumber;
        this.timestamp = timestamp;
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o)
        {
            return true;
        }
        if (o == null || getClass() != o.getClass())
        {
            return false;
        }

        SnapshotVersionDecomposition that = (SnapshotVersionDecomposition) o;

        return version != null ? version.equals(that.version) : that.version == null;
    }

    @Override
    public int hashCode()
    {
        return version != null ? version.hashCode() : 0;
    }

    public String getVersion()
    {
        return version;
    }

    public int getBuildNumber()
    {
        return buildNumber;
    }

    public String getTimestamp()
    {
        return timestamp;
    }
}
