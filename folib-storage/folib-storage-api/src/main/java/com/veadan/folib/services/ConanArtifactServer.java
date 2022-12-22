package com.veadan.folib.services;

import com.veadan.folib.storage.repository.Repository;
import org.springframework.http.ResponseEntity;

public interface ConanArtifactServer {

    //查询conan 仓制品包
    ResponseEntity searchConanPackage(Repository repository, String query);

    ResponseEntity downloadUrls(Repository repository, String packageName, String version);

    ResponseEntity searchConanPackageInfo(Repository repository, String packageName, String version);
}
