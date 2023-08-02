package com.veadan.folib.controllers.layout.cocoapods;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.providers.io.LayoutFileSystemFactory;
import com.veadan.folib.providers.io.RepositoryPath;
import com.veadan.folib.providers.layout.CocoapodsFileSystem;
import com.veadan.folib.providers.layout.IndexingDisabledException;
import com.veadan.folib.providers.layout.MavenFileSystem;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.web.RepositoryMapping;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;
import java.io.IOException;

import static com.veadan.folib.config.Maven2LayoutProviderConfig.FILE_SYSTEM_ALIAS;

/***
 *
 * @author xiaodong.wang
 * @email wangxiaodong@beyondcent.com
 * @date 2023/8/2 14:58
 * @since x.x.x
 */
@RestController
@RequestMapping("/api/cocoapods/index")
public class CocoapodsIndexController
        extends BaseController {

    @Inject
    @Qualifier(FILE_SYSTEM_ALIAS)
    private LayoutFileSystemFactory layoutFileSystemFactory;

    //    @PreAuthorize("hasAuthority('MANAGEMENT_REBUILD_INDEXES')")
    @GetMapping(value = "/{storageId}/{repositoryId}")
    public void repoArtIndex(@RepositoryMapping Repository repository) {
        
        LayoutFileSystem layoutFileSystem = layoutFileSystemFactory.create(repository);
        final RepositoryPath path = layoutFileSystem.getPath("/.specs");
        path.iterator().forEachRemaining(e -> {
            System.out.println(e.getFileName());
        });
    }
}
