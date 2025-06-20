package com.veadan.folib.controllers;

import com.veadan.folib.dto.component.ArtifactGraphDto;
import com.veadan.folib.dto.component.ArtifactStatisticsDto;
import com.veadan.folib.dto.component.ComponentTableDto;
import com.veadan.folib.dto.vulnerability.AffectedArtifactsDto;
import com.veadan.folib.scanner.common.msg.TableResultResponse;
import com.veadan.folib.services.ComponentService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.inject.Inject;

/**
 * @author leipenghui
 */
@RestController
@RequestMapping("/api/component")
@Api(description = "组件管理",tags = "分页组件管理")
public class ComponentController extends BaseController {

    @Inject
    private ComponentService componentService;

    @ApiOperation(value = "查询组件分页列表", response = ComponentTableDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/page")
    public TableResultResponse<ComponentTableDto> page(@RequestParam(name = "page", required = false) Integer page,
                                                       @RequestParam(name = "limit", required = false) Integer limit,
                                                       @RequestParam(name = "name", required = false) String name,
                                                       @RequestParam(name = "groupId", required = false) String groupId,
                                                       @RequestParam(name = "version", required = false) String version,
                                                       @RequestParam(name = "searchKeyword", required = false) String searchKeyword) {
        return componentService.queryComponentPage(page, limit, name, groupId, version, searchKeyword);
    }

    @ApiOperation(value = "查询组件分页列表", response = ComponentTableDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/pageByArtifact")
    public TableResultResponse<ComponentTableDto> pageByArtifact(@RequestParam(name = "page", required = false) Integer page,
                                                                 @RequestParam(name = "limit", required = false) Integer limit,
                                                                 @RequestParam(name = "artifactPath") String artifactPath,
                                                                 @RequestParam(name = "searchKeyword", required = false) String searchKeyword) {
        return componentService.queryComponentPageByArtifact(page, limit, artifactPath, searchKeyword);
    }

    @ApiOperation(value = "查询组件信息", response = ComponentTableDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/single")
    public ResponseEntity<ComponentTableDto> queryComponentOne(@RequestParam(name = "uuid") String uuid) {
        return ResponseEntity.ok(componentService.queryComponentOne(uuid));
    }

    @ApiOperation(value = "根据组件id分页查询关联制品", response = AffectedArtifactsDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/artifactPage")
    public TableResultResponse<AffectedArtifactsDto> artifactPage(@RequestParam(name = "page", required = false) Integer page,
                                                                  @RequestParam(name = "limit", required = false) Integer limit,
                                                                  @RequestParam(name = "componentUuid") String componentUuid,
                                                                  @RequestParam(name = "searchKeyword", required = false) String searchKeyword) {
        return componentService.queryArtifactByComponentUuid(page, limit, componentUuid, searchKeyword);
    }

    @ApiOperation(value = "根据组件id分页查询关联制品", response = ArtifactGraphDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/artifactGraph")
    public ResponseEntity<ArtifactGraphDto> artifactPage(@RequestParam(name = "componentUuid") String componentUuid) {
        return ResponseEntity.ok(componentService.artifactGraph(componentUuid));
    }

    @ApiOperation(value = "组件关联的制品统计数据", response = ArtifactStatisticsDto.class)
    @ApiResponses(value = {@ApiResponse(code = 200, message = "OK")})
    @PreAuthorize("hasAuthority('COMPONENTS_VIEW')")
    @GetMapping(value = "/artifactStatistics")
    public ResponseEntity<ArtifactStatisticsDto> artifactStatistics(@RequestParam(name = "componentUuid") String componentUuid) {
        return ResponseEntity.ok(componentService.artifactStatistics(componentUuid));
    }
}
