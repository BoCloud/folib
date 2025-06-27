package com.veadan.folib.controllers;

import com.veadan.folib.constant.GlobalConstants;
import com.veadan.folib.entity.Resource;
import com.veadan.folib.users.service.ResourceService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author veadan
 */
@RestController
@RequestMapping("/api/resource")
@Api(value = "资源管理",tags = "资源管理")
public class ResourceController extends BaseController {

    public static final String NOT_FOUND_RESOURCE = "资源不存在!";

    @Inject
    private ResourceService resourceService;

    @ApiOperation(value = "获取用户的关联角色")
    @ApiResponses(value = { @ApiResponse(code = 200, message = "Returns resource list"),
                            @ApiResponse(code = 403, message = "Unauthenticated access or user account has been disabled"),
                            @ApiResponse(code = 404, message = NOT_FOUND_RESOURCE) })
    @PreAuthorize("hasAuthority('AUTHENTICATED_RESOURCE')")
    @GetMapping
    @ResponseBody
    public ResponseEntity getAccount(@RequestParam(value = "resourceType") String resourceType)
    {
        List<Resource> allResource = resourceService.findAll();
        List<Resource> data = new ArrayList<>(allResource.size());
        switch(resourceType) {
            case GlobalConstants.RESOURCE_TYPE_API:
                data = allResource.stream().filter(resource -> StringUtils.isNotBlank(resource.getApiAuthoritie())).collect(Collectors.toList());
                break;
            case GlobalConstants.RESOURCE_TYPE_STORAGE:
                data = allResource.stream().filter(resource -> StringUtils.isNotBlank(resource.getStorageId())).collect(Collectors.toList());
                break;
            case GlobalConstants.RESOURCE_TYPE_REPOSITORY:
                data = allResource.stream().filter(resource -> StringUtils.isNotBlank(resource.getRepositoryId())).collect(Collectors.toList());
                break;
            case GlobalConstants.RESOURCE_TYPE_PATH:
                data = allResource.stream().filter(resource -> StringUtils.isNotBlank(resource.getPath())).collect(Collectors.toList());
                break;
            default:
                data.addAll(allResource);
        }

        return ResponseEntity.ok(data);
    }

}
