package com.folib.services.support;

/**
 * @author veadan
 */
public class ArtifactEntrySearchCriteria
{

    private Integer lastAccessedTimeInDays;

    private Long minSizeInBytes;

    public boolean isEmpty()
    {
        return lastAccessedTimeInDays == null && minSizeInBytes == null;
    }

    public Integer getLastAccessedTimeInDays()
    {
        return lastAccessedTimeInDays;
    }

    public Long getMinSizeInBytes()
    {
        return minSizeInBytes;
    }

    public static final class Builder
    {

        private Integer lastAccessedTimeInDays;
        private Long minSizeInBytes;

        private Builder()
        {
        }

        public static Builder anArtifactEntrySearchCriteria()
        {
            return new Builder();
        }

        public Builder withLastAccessedTimeInDays(Integer lastAccessedTimeInDays)
        {
            this.lastAccessedTimeInDays = lastAccessedTimeInDays;
            return this;
        }

        public Builder withMinSizeInBytes(Long minSizeInBytes)
        {
            this.minSizeInBytes = minSizeInBytes;
            return this;
        }

        public ArtifactEntrySearchCriteria build()
        {
            ArtifactEntrySearchCriteria artifactEntrySearchCriteria = new ArtifactEntrySearchCriteria();
            artifactEntrySearchCriteria.lastAccessedTimeInDays = this.lastAccessedTimeInDays;
            artifactEntrySearchCriteria.minSizeInBytes = this.minSizeInBytes;
            return artifactEntrySearchCriteria;
        }
    }
}
