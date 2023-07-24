package com.veadan.folib.services;

import com.veadan.folib.storage.repository.Repository;
import org.springframework.http.ResponseEntity;

/**
 * @author Veadan
 */
public interface ConanArtifactServer {

    /**
     * 查询conan仓库制品包
     *
     * @param repository repository
     * @param query      query
     * @return 制品包
     */
    ResponseEntity<?> searchConanPackage(Repository repository, String query);

    /**
     * downloadUrls
     *
     * @param repository repository
     * @param name       name
     * @param version    version
     * @param user       user
     * @param channel    channel
     * @return downloadUrls
     */
    ResponseEntity<?> downloadUrls(Repository repository, String name, String version, String user, String channel);

    /**
     * searchConanPackageInfo
     *
     * @param repository repository
     * @param name       name
     * @param version    version
     * @param user       user
     * @param channel    channel
     * @return searchConanPackageInfo
     */
    ResponseEntity<?> searchConanPackageInfo(Repository repository, String name, String version, String user, String channel);
}
