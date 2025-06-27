package com.veadan.folib.controllers.layout.debian;

import com.veadan.folib.annotation.AuditLog;
import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.DebianArtifactCoordinatesAdapter;
import com.veadan.folib.artifact.coordinates.GenericArtifactCoordinates;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.db.schema.Edges;
import com.veadan.folib.db.schema.Properties;
import com.veadan.folib.db.schema.Vertices;
import com.veadan.folib.domain.Artifact;
import com.veadan.folib.domain.debian.DebianParserVO;
import com.veadan.folib.domain.debian.DebianUploadBO;
import com.veadan.folib.enums.AuditEventNameEnum;
import com.veadan.folib.gremlin.adapters.ArtifactAdapter;
import com.veadan.folib.gremlin.adapters.ArtifactCoordinatesHierarchyAdapter;
import com.veadan.folib.gremlin.adapters.EntityTraversalAdapter;
import com.veadan.folib.gremlin.dsl.EntityTraversal;
import com.veadan.folib.gremlin.repositories.GremlinVertexRepository;
import com.veadan.folib.services.DebianService;
import com.veadan.folib.storage.repository.Repository;
import org.apache.tinkerpop.gremlin.process.traversal.P;
import org.apache.tinkerpop.gremlin.structure.Edge;
import org.apache.tinkerpop.gremlin.structure.Vertex;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.inject.Inject;
import java.util.List;
import java.util.Optional;

/**
 * @author veadan
 * @since 2024-09-06 10:36
 */


@RestController
@RequestMapping("api/debian")
public class DebianStorageController{

    @Resource
    private DebianService debianService;

    @PostMapping("parseArtifact")
    public ResponseEntity<DebianParserVO> parseArtifact(String storageId, String repositoryId, MultipartFile file) {
        return ResponseEntity.ok(debianService.parseArtifact(storageId, repositoryId, file));
    }

    @AuditLog(value = AuditEventNameEnum.UPLOAD_ARTIfFACT,target ="#uploadBO.getStorageId() + '/'+ #uploadBO.getRepositoryId() + '/' + #uploadBO.getPath() + '/' + #uploadBO.getFileName()" )
    @PostMapping("upload")
    public ResponseEntity<String> upload(@Validated @RequestBody DebianUploadBO uploadBO) {
        return ResponseEntity.ok(debianService.upload(uploadBO));
    }
    @PostMapping("batchUpload")
    public ResponseEntity<String> batchUpload(List<MultipartFile> files, String storageId, String repositoryId, String distribution,String component) {
        debianService.batchUpload(files,storageId,repositoryId,distribution,component);
        return ResponseEntity.ok("accepted");
    }






}
