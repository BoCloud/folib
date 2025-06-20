package com.veadan.folib.storage.indexing;

import com.veadan.folib.storage.repository.Repository;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.store.Directory;
import org.apache.maven.index.artifact.GavCalculator;
import org.apache.maven.index.context.DocumentFilter;
import org.apache.maven.index.context.IndexCreator;
import org.apache.maven.index.context.IndexingContext;

/**
 * @author veadan
 */
public class RepositoryCloseableIndexingContext
        implements IndexingContext, Closeable
{

    private final IndexingContext indexingContext;

    private final Repository repository;

    public RepositoryCloseableIndexingContext(IndexingContext indexingContext,
                                              Repository repository)
    {
        this.indexingContext = indexingContext;
        this.repository = repository;
    }

    @Override
    public String getId()
    {
        return indexingContext.getId();
    }

    @Override
    public String getRepositoryId()
    {
        return indexingContext.getRepositoryId();
    }

    @Override
    public File getRepository()
    {
        return indexingContext.getRepository();
    }

    @Override
    public String getRepositoryUrl()
    {
        return indexingContext.getRepositoryUrl();
    }

    @Override
    public String getIndexUpdateUrl()
    {
        return indexingContext.getIndexUpdateUrl();
    }

    @Override
    public boolean isSearchable()
    {
        return indexingContext.isSearchable();
    }

    @Override
    public void setSearchable(boolean searchable)
    {
        indexingContext.setSearchable(searchable);
    }

    @Override
    public Date getTimestamp()
    {
        return indexingContext.getTimestamp();
    }

    @Override
    public void updateTimestamp()
            throws IOException
    {
        indexingContext.updateTimestamp();
    }

    @Override
    public void updateTimestamp(boolean save)
            throws IOException
    {
        indexingContext.updateTimestamp(save);
    }

    @Override
    public void updateTimestamp(boolean save,
                                Date date)
            throws IOException
    {
        indexingContext.updateTimestamp(save, date);
    }

    @Override
    public int getSize()
            throws IOException
    {
        return indexingContext.getSize();
    }

    @Override
    public IndexSearcher acquireIndexSearcher()
            throws IOException
    {
        return indexingContext.acquireIndexSearcher();
    }

    @Override
    public void releaseIndexSearcher(IndexSearcher s)
            throws IOException
    {
        indexingContext.releaseIndexSearcher(s);
    }

    @Override
    public IndexWriter getIndexWriter()
            throws IOException
    {
        return indexingContext.getIndexWriter();
    }

    @Override
    public List<IndexCreator> getIndexCreators()
    {
        return indexingContext.getIndexCreators();
    }

    @Override
    public Analyzer getAnalyzer()
    {
        return indexingContext.getAnalyzer();
    }

    @Override
    public void commit()
            throws IOException
    {
        indexingContext.commit();
    }

    @Override
    public void rollback()
            throws IOException
    {
        indexingContext.rollback();
    }

    @Override
    public void optimize()
            throws IOException
    {
        indexingContext.optimize();
    }

    @Override
    public void close(boolean deleteFiles)
            throws IOException
    {
        indexingContext.close(deleteFiles);
    }

    @Override
    public void purge()
            throws IOException
    {
        indexingContext.purge();
    }

    @Override
    public void merge(Directory directory)
            throws IOException
    {
        indexingContext.merge(directory);
    }

    @Override
    public void merge(Directory directory,
                      DocumentFilter filter)
            throws IOException
    {
        indexingContext.merge(directory, filter);
    }

    @Override
    public void replace(Directory directory)
            throws IOException
    {
        indexingContext.replace(directory);
    }

    @Override
    public void replace(Directory directory,
                        Set<String> allGroups,
                        Set<String> rootGroups)
            throws IOException
    {
        indexingContext.replace(directory, allGroups, rootGroups);
    }

    @Override
    public Directory getIndexDirectory()
    {
        return indexingContext.getIndexDirectory();
    }

    @Override
    public File getIndexDirectoryFile()
    {
        return indexingContext.getIndexDirectoryFile();
    }

    @Override
    public GavCalculator getGavCalculator()
    {
        return indexingContext.getGavCalculator();
    }

    @Override
    public void setAllGroups(Collection<String> groups)
            throws IOException
    {
        indexingContext.setAllGroups(groups);
    }

    @Override
    public Set<String> getAllGroups()
            throws IOException
    {
        return indexingContext.getAllGroups();
    }

    @Override
    public void setRootGroups(Collection<String> groups)
            throws IOException
    {
        indexingContext.setRootGroups(groups);
    }

    @Override
    public Set<String> getRootGroups()
            throws IOException
    {
        return indexingContext.getRootGroups();
    }

    @Override
    public void rebuildGroups()
            throws IOException
    {
        indexingContext.rebuildGroups();
    }

    @Override
    public boolean isReceivingUpdates()
    {
        return indexingContext.isReceivingUpdates();
    }

    @Override
    public void close()
            throws IOException
    {
        close(false);
    }

    public Repository getRepositoryRaw()
    {
        return repository;
    }
}
