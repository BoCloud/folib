package com.folib.index;

import com.google.common.collect.ArrayListMultimap;
import com.folib.utils.MlModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MlModelRemoteEtagIndex {

    private static final Logger log = LoggerFactory.getLogger(MlModelRemoteEtagIndex.class);


    public MlModelRemoteEtagIndex() {
    }

    public void indexEtagOnArtifact( String storageId, String repositoryId,  String path,  String etag)  {
        if (repositoryId == null) {
            throw new NullPointerException("repositoryId is marked non-null but is null");
        }
        if (path == null) {
            throw new NullPointerException("path is marked non-null but is null");
        }
        if (etag == null) {
            throw new NullPointerException("etag is marked non-null but is null");
        }
        ArrayListMultimap<Object, Object> arrayListMultimap = ArrayListMultimap.create();
        arrayListMultimap.put("huggingfaceml.etag.file", MlModelUtils.removeQuote(etag));
        try {
            log.debug("About to try to set attribute {} with value: {}  on repo: {} path {}", "huggingfaceml.etag.file", etag, repositoryId, path );
            // TODO: 2024/6/20
            //this.securityService.callAsSystem(() -> {
            //    this.repositoryService.setAttributes(repoKey, path, attributes);
            //    return null;
            //});
        } catch (Exception e) {
            log.error("Could not set the attribute {} with value: {} on repo: {} path {} message:{}",  "huggingfaceml.etag.file", etag, repositoryId, path, e
                    .getMessage() );
            log.debug("Could not set the attribute {} with value: {} on repo: {} path {}",  "huggingfaceml.etag.file", etag, repositoryId, path, e );
        }
    }
}

