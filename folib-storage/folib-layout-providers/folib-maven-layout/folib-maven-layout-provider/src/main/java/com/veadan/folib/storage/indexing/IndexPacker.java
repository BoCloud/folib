package com.veadan.folib.storage.indexing;

import com.veadan.folib.providers.io.RepositoryPath;

import java.io.IOException;
import java.nio.file.Files;

import org.apache.lucene.search.IndexSearcher;
import org.apache.maven.index.context.IndexingContext;
import org.apache.maven.index.incremental.DefaultIncrementalHandler;
import org.apache.maven.index.packer.DefaultIndexPacker;
import org.apache.maven.index.packer.IndexPackingRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class IndexPacker
{

    private static final Logger logger = LoggerFactory.getLogger(IndexPacker.class);

    private static final org.apache.maven.index.packer.IndexPacker INSTANCE = new DefaultIndexPacker(
            new DefaultIncrementalHandler());

    public static RepositoryPath pack(final RepositoryPath indexPath,
                                      final IndexingContext context)
            throws IOException
    {
        final IndexSearcher indexSearcher = context.acquireIndexSearcher();
        try
        {

            final IndexPackingRequest request = new IndexPackingRequest(context,
                                                                        indexSearcher.getIndexReader(),
                                                                        indexPath.toFile());
            request.setUseTargetProperties(true);
            IndexPacker.INSTANCE.packIndex(request);

            logger.info("Index for {} was packed successfully.", indexPath);
        }
        finally
        {
            context.releaseIndexSearcher(indexSearcher);
        }
        return indexPath.resolve(IndexingContext.INDEX_FILE_PREFIX + ".gz");
    }

    public static boolean packageExists(final RepositoryPath indexPath)
    {
        return Files.exists(indexPath.resolve(IndexingContext.INDEX_FILE_PREFIX + ".gz"));
    }
}
