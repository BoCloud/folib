package com.veadan.folib.storage.indexing.local;

/**
 * @author Przemyslaw Fusik
 */
public class ArtifactEntryArtifactContextHelper
{

    private final boolean pomExists;

    private final boolean sourcesExists;

    private final boolean javadocExists;


    public ArtifactEntryArtifactContextHelper(boolean pomExists,
                                              boolean sourcesExists,
                                              boolean javadocExists)
    {
        this.pomExists = pomExists;
        this.sourcesExists = sourcesExists;
        this.javadocExists = javadocExists;
    }

    public boolean pomExists()
    {
        return pomExists;
    }

    public boolean sourcesExists()
    {
        return sourcesExists;
    }

    public boolean javadocExists()
    {
        return javadocExists;
    }
}
