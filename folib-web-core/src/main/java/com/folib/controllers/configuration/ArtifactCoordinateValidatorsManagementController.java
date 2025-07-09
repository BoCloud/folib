package com.folib.controllers.configuration;

import com.folib.storage.validation.ArtifactCoordinatesValidator;
import com.folib.storage.validation.artifact.ArtifactCoordinatesValidatorRegistry;
import com.folib.controllers.BaseController;
import com.folib.storage.repository.Repository;
import com.folib.web.RepoMapping;

import javax.inject.Inject;

import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

/**
 * @author veadan
 * @author Veadan
 * @author Aditya Srinivasan
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = "/api/configuration/artifact-coordinate-validators")
@Api(description = "制品存储空间坐标验证器",tags = "制品存储空间坐标验证器")
public class ArtifactCoordinateValidatorsManagementController
        extends BaseController
{

    static final String SUCCESSFUL_LIST = "所请求存储库的所有版本验证器";
    
    static final String NOT_FOUND_STORAGE_MESSAGE = "找不到请求的存储空间 ${storageId}.";
    
    static final String NOT_FOUND_REPOSITORY_MESSAGE = "找不到请求的存储库 ${storageId}:${repositoryId}.";
    
    static final String NOT_FOUND_LAYOUT_PROVIDER_MESSAGE = "找不到布局提供程序的请求工件坐标验证器 ${layoutProvider}.";

    static final String SUCCESSFUL_ADD = "版本验证器类型已添加到请求的存储库中.";

    static final String SUCCESSFUL_DELETE = "版本验证器类型已从请求的存储库中删除.";
    
    static final String NOT_FOUND_ALIAS_MESSAGE = "无法从请求的存储库中删除请求的别名.";

    @Inject
    private ArtifactCoordinatesValidatorRegistry artifactCoordinatesValidatorRegistry;
    
    
    @ApiOperation(value = "枚举所请求存储库的所有版本验证器")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_LIST),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY_MESSAGE) })
    @GetMapping(value = "/{storageId}/{repositoryId}",
                produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity listArtifactCoordinatesForRepository(@RepoMapping Repository repository)
    {
        Set<String> versionValidators = repository.getArtifactCoordinateValidators()
                                                  .stream()
                                                  .map(String::toString)
                                                  .collect(Collectors.toSet());

        return getJSONListResponseEntityBody("versionValidators", versionValidators);
    }

    @ApiOperation(value = "将版本验证器类型添加到请求的存储库")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_ADD),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY_MESSAGE) })
    @PutMapping(value = "/{storageId}/{repositoryId}/{alias}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity add(@RepoMapping Repository repository,
                              @PathVariable String alias,
                              @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        configurationManagementService.addRepositoryArtifactCoordinateValidator(storageId, repositoryId, alias);

        return getSuccessfulResponseEntity(SUCCESSFUL_ADD, acceptHeader);
    }

    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_DELETE),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY_MESSAGE) })
    @DeleteMapping(value = "/{storageId}/{repositoryId}/{alias}",
                   produces = { MediaType.TEXT_PLAIN_VALUE,
                                MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity delete(@RepoMapping Repository repository,
                                 @PathVariable String alias,
                                 @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader) throws IOException
    {
        final String storageId = repository.getStorage().getId();
        final String repositoryId = repository.getId();

        boolean resultOk = configurationManagementService.removeRepositoryArtifactCoordinateValidator(storageId,
                                                                                                      repositoryId,
                                                                                                      alias);
        if (!resultOk)
        {
            return getNotFoundResponseEntity(NOT_FOUND_ALIAS_MESSAGE, acceptHeader);
        }

        return getSuccessfulResponseEntity(SUCCESSFUL_DELETE, acceptHeader);
    }

    @ApiOperation(value = "返回注册表中所有可用工件坐标验证器的列表")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_LIST),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY_MESSAGE) })
    @GetMapping(value = "/validators",
                produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity listArtifactCoordinateValidators()
    {
        return getJSONListResponseEntityBody("versionValidators",
                                             artifactCoordinatesValidatorRegistry.getArtifactCoordinatesValidators()
                                                                                 .entrySet());
    }

    @ApiOperation(value = "返回给定布局提供程序支持的工件坐标验证器列表")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_LIST),
                            @ApiResponse(code = 404, message = NOT_FOUND_REPOSITORY_MESSAGE) })
    @GetMapping(value = "/validators/{layoutProvider}",
                produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity listArtifactCoordinateValidatorsForLayoutProvider(@PathVariable String layoutProvider,
                                                                            @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        Map<String, Set<ArtifactCoordinatesValidator>> validators = artifactCoordinatesValidatorRegistry.getArtifactCoordinatesValidators();

        if (!validators.containsKey(layoutProvider))
        {
            return getNotFoundResponseEntity(NOT_FOUND_LAYOUT_PROVIDER_MESSAGE, acceptHeader);
        }
        
        return getJSONListResponseEntityBody("versionValidators", validators.get(layoutProvider));
    }
    
}
