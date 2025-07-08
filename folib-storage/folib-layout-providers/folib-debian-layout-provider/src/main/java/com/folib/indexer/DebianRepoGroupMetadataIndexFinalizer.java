package com.folib.indexer;

import com.folib.storage.repository.Repository;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

/**
 * @author veadan
 * @since 2025-03-07 10:13
 */
@Slf4j
@Data
public class DebianRepoGroupMetadataIndexFinalizer {

    private Repository groupRepo;
    private List<String> compFoldersToDelete;
    private String distribution;

    public DebianRepoGroupMetadataIndexFinalizer( Repository groupRepo, String distribution, List<String> packagesFilesToDelete) {
        this.distribution=distribution;
        this.compFoldersToDelete = packagesFilesToDelete;
        this.groupRepo = groupRepo;
    }
    public void finalizeIndex() {
//        String source = this.tempPathRoot + "/" + this.distribution + "/";
//        String target = "dists" + this.resolveParentDist(this.distribution);
//        long move = System.currentTimeMillis();
//        log.debug("Moving calculated index from temp location {} to location {}", source, target);
//        ((DebianRepoWorkContext)this.repo.getWorkContext()).setSystem();
//        ((DebianService)ContextHelper.get().beanForType(DebianService.class)).finalizeVirtualIndexing(this.repo, this.virtualRepo, source, target, this.distribution, this.compFoldersToDelete);
//        log.trace("Finished move to location {}. took {}ms", target, DpkgUtils.time(move));
    }

    public void removeTemp() {

    }
}
