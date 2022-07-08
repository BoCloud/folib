package com.veadan.folib.providers.repository;

import com.veadan.folib.data.criteria.Paginator;
import com.veadan.folib.storage.repository.RepositoryDto;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;
import java.util.List;

import org.springframework.transaction.annotation.Transactional;

/**
 * This interface provide functionality to operate with artifact Paths.
 * Implementation depends of {@link Repository} type which can be: Hosted, Group
 * or Proxy.
 * 
 * TODO: should be replaced with `RepositoryFileSystemProvider`
 * 
 * @author Veadan
 */
public interface RepositoryProvider
{

    /**
     * Return {@link RepositoryDto} type alias.
     * 
     * @return
     */
    String getAlias();

    /**
     * Return {@link InputStream} to read Artifact content.
     * 
     * @param path
     * @return
     * @throws IOException
     */
    InputStream getInputStream(Path path) throws IOException;
    
    /**
     * Return {@link OutputStream} to write Artifact content.
     * 
     * @param path
     * @return
     * @throws IOException
     * @throws NoSuchAlgorithmException
     */
    OutputStream getOutputStream(Path path)
            throws IOException, NoSuchAlgorithmException;
    
    /**
     * Searches Artifact Paths. For Group Repositories result will be group
     * member Paths.
     * 
     * @param storageId
     * @param repositoryId
     * @param predicate
     * @param paginator
     * @return
     */
    @Transactional(readOnly = true)
    List<Path> search(String storageId,
                      String repositoryId,
                      RepositorySearchRequest predicate,
                      Paginator paginator);
    
    /**
     * Counts Artifacts. For Group repositories result will be distinct within
     * group members.
     * 
     * @param storageId
     * @param repositoryId
     * @param predicate
     * @return
     */
    @Transactional(readOnly = true)
    Long count(String storageId,
               String repositoryId,
               RepositorySearchRequest predicate);
    
    /**
     * Fetch Artifact Path from target repository.
     * For Group repository it will resolve Path from underlying group member.
     * For Proxy repository it will try to download remote Artifact if it's not cached.
     * Return  <code>null<code> if there is no such Path in target repository.
     * 
     * To resolve target path you should use {@link RepositoryPathResolver}
     * 
     * @param repositoryPath
     * @return
     * @throws IOException
     */
    Path fetchPath(Path repositoryPath)
        throws IOException;

}
