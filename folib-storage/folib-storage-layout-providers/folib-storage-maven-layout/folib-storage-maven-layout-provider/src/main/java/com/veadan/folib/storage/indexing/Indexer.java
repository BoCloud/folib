package com.veadan.folib.storage.indexing;

import org.apache.maven.index.DefaultIndexer;
import org.apache.maven.index.DefaultIndexerEngine;
import org.apache.maven.index.DefaultQueryCreator;
import org.apache.maven.index.DefaultSearchEngine;

/**
 * @author veadan
 */
public class Indexer
{

    public static final org.apache.maven.index.Indexer INSTANCE = new DefaultIndexer(new DefaultSearchEngine(),
                                                                                     new DefaultIndexerEngine(),
                                                                                     new DefaultQueryCreator());
}
