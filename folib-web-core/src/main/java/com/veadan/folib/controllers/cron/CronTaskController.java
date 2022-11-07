package com.veadan.folib.controllers.cron;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.cron.domain.CronTaskConfigurationDto;
import com.veadan.folib.cron.domain.CronTasksConfigurationDto;
import com.veadan.folib.cron.domain.GroovyScriptNamesDto;
import com.veadan.folib.cron.jobs.CronJobDefinition;
import com.veadan.folib.cron.jobs.CronJobsDefinitionsRegistry;
import com.veadan.folib.cron.jobs.GroovyCronJob;
import com.veadan.folib.cron.services.CronJobSchedulerService;
import com.veadan.folib.cron.services.CronTaskConfigurationService;
import com.veadan.folib.forms.cron.CronTaskConfigurationForm;
import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.validation.RequestBodyValidationException;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.core.convert.ConversionService;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * Defines cron task processing API.
 *
 * @author 
 * @author Veadan
 */
@Controller
@RequestMapping("/api/configuration/crontasks")
@PreAuthorize("hasAuthority('ADMIN')")
public class CronTaskController
        extends BaseController
{

    static final String HEADER_NAME_CRON_TASK_ID = "X-CRON-TASK-ID";

    static final String CRON_CONFIG_FILE_NAME_KEY = "fileName";
    static final String CRON_CONFIG_SCRIPT_PATH_KEY = "script.path";

    private static final String SUCCESSFUL_SAVE_CONFIGURATION = "配置保存成功.";
    private static final String FAILED_SAVE_CONFIGURATION = "无法保存定时配置.";

    private static final String SUCCESSFUL_UPDATE_CONFIGURATION = "定时配置更新成功.";
    private static final String FAILED_UPDATE_CONFIGURATION = "无法更新定时配置.";

    private static final String SUCCESSFUL_DELETE_CONFIGURATION = "删除定时配置成功.";
    private static final String FAILED_DELETE_CONFIGURATION = "无法删除配置配置.";

    private static final String SUCCESSFUL_GET_CONFIGURATIONS = "配置检索成功.";
    private static final String NOT_FOUND_CONFIGURATIONS = "没有 cron 任务配置";

    private static final String SUCCESSFUL_GET_CONFIGURATION = "配置检索成功.";
    private static final String NOT_FOUND_CONFIGURATION = "此 uuid 未找到 Cron 任务配置!";

    private static final String SUCCESSFUL_UPLOAD_GROOVY_SCRIPT = "groovy脚本上传成功.";
    private static final String FAILED_UPLOAD_GROOVY_SCRIPT = "无法上传 groovy 脚本.";

    private static final String SUCCESSFUL_GET_GROOVY_SCRIPTS = "成功检索名为的 groovy 脚本.";

    @Inject
    private CronTaskConfigurationService cronTaskConfigurationService;

    @Inject
    private CronJobsDefinitionsRegistry cronJobsDefinitionsRegistry;

    @Inject
    private CronJobSchedulerService cronJobSchedulerService;

    @Inject
    private ConversionService conversionService;

    @Inject
    private PropertiesBooter propertiesBooter;


    @ApiOperation(value = "Used to save a new cron task job")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_SAVE_CONFIGURATION),
                            @ApiResponse(code = 400, message = FAILED_SAVE_CONFIGURATION) })
    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity create(@RequestBody @Validated CronTaskConfigurationForm cronTaskConfigurationForm,
                                 BindingResult bindingResult,
                                 @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_SAVE_CONFIGURATION, bindingResult);
        }

        try
        {
            CronTaskConfigurationDto cronTaskConfiguration = conversionService.convert(cronTaskConfigurationForm,
                                                                                       CronTaskConfigurationDto.class);
            UUID uuid = cronTaskConfigurationService.saveConfiguration(cronTaskConfiguration);

            HttpHeaders httpHeaders = new HttpHeaders();
            httpHeaders.add(HEADER_NAME_CRON_TASK_ID, uuid.toString());

            return getSuccessfulResponseEntity(SUCCESSFUL_SAVE_CONFIGURATION,
                                               httpHeaders,
                                               acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR,
                                              FAILED_SAVE_CONFIGURATION,
                                              e,
                                              acceptHeader);
        }
    }

    @ApiOperation(value = "Used to update an existing configuration")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_UPDATE_CONFIGURATION),
                            @ApiResponse(code = 400, message = FAILED_UPDATE_CONFIGURATION) })
    @PutMapping(value = "/{UUID}",
                consumes = MediaType.APPLICATION_JSON_VALUE,
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity updateConfiguration(@PathVariable("UUID") UUID uuid,
                                              @RequestBody @Validated CronTaskConfigurationForm cronTaskConfigurationForm,
                                              BindingResult bindingResult,
                                              @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        if (bindingResult.hasErrors())
        {
            throw new RequestBodyValidationException(FAILED_UPDATE_CONFIGURATION, bindingResult);
        }

        CronTaskConfigurationDto configuration = cronTaskConfigurationService.getTaskConfigurationDto(uuid);
        if (configuration == null)
        {
            return getBadRequestResponseEntity(FAILED_UPDATE_CONFIGURATION, acceptHeader);
        }

        try
        {
            CronTaskConfigurationDto cronTaskConfiguration = conversionService.convert(cronTaskConfigurationForm,
                                                                                       CronTaskConfigurationDto.class);
            cronTaskConfiguration.setUuid(uuid);
            cronTaskConfigurationService.saveConfiguration(cronTaskConfiguration);

            return getSuccessfulResponseEntity(SUCCESSFUL_SAVE_CONFIGURATION, acceptHeader);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR,
                                              FAILED_SAVE_CONFIGURATION,
                                              e,
                                              acceptHeader);
        }
    }

    @ApiOperation(value = "Used to delete the configuration")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_DELETE_CONFIGURATION),
                            @ApiResponse(code = 400, message = FAILED_DELETE_CONFIGURATION) })
    @DeleteMapping(value = "{UUID}",
            produces = { MediaType.TEXT_PLAIN_VALUE,
                         MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity deleteConfiguration(@PathVariable("UUID") UUID uuid,
                                              @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        final CronTaskConfigurationDto config = cronTaskConfigurationService.getTaskConfigurationDto(uuid);
        if (config == null)
        {
            return getNotFoundResponseEntity(NOT_FOUND_CONFIGURATION, acceptHeader);
        }

        try
        {
            cronTaskConfigurationService.deleteConfiguration(config.getUuid());
            if (config.getJobClass().equals(GroovyCronJob.class.getName()) &&
                config.getProperty(CRON_CONFIG_SCRIPT_PATH_KEY) != null)
            {
                Path path = Paths.get(config.getProperty(CRON_CONFIG_SCRIPT_PATH_KEY));
                Files.deleteIfExists(path);
            }
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR,
                                              FAILED_DELETE_CONFIGURATION,
                                              e,
                                              acceptHeader);
        }


        final String message = String.format("Configuration %s removed", uuid);
        logger.debug(message);

        return getSuccessfulResponseEntity(SUCCESSFUL_DELETE_CONFIGURATION, acceptHeader);
    }

    @ApiOperation(value = "列出所有 cron 任务类型和这些任务的字段类型.")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_CONFIGURATION),
                            @ApiResponse(code = 404, message = NOT_FOUND_CONFIGURATION) })
    @GetMapping(value = "/types/list",
                produces = { MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity listCronJobs(@RequestParam(value = "scope",required = false) String scope)
    {
        Set<CronJobDefinition> cronJobDefinitions=cronJobsDefinitionsRegistry.getCronJobDefinitions();
        if(StringUtils.isNotBlank(scope)){
            cronJobDefinitions= cronJobsDefinitionsRegistry.getCronJobDefinitions().stream()
                    .filter(cjd -> cjd.getScope().equals(scope)||cjd.getScope().equals("GLOBAL")).collect(Collectors.toSet());
        }
        return ResponseEntity.ok(cronJobDefinitions);
    }

    @ApiOperation(value = "Used to get the configuration on given cron task UUID")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_CONFIGURATION),
                            @ApiResponse(code = 404, message = NOT_FOUND_CONFIGURATION) })
    @GetMapping(value = "/{UUID}",
                produces = { MediaType.APPLICATION_JSON_VALUE,
                             com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE })
    public ResponseEntity getConfiguration(@PathVariable("UUID") UUID uuid,
                                           @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        CronTaskConfigurationDto config = cronTaskConfigurationService.getTaskConfigurationDto(uuid);
        if (config == null)
        {
            return getNotFoundResponseEntity(NOT_FOUND_CONFIGURATION, acceptHeader);
        }

        return ResponseEntity.ok(config);
    }

    @ApiOperation(value = "获取所有已经设置的定时任务")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_CONFIGURATIONS),
                            @ApiResponse(code = 404, message = NOT_FOUND_CONFIGURATIONS) })
    @GetMapping(produces = { MediaType.APPLICATION_JSON_VALUE,
                             com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE })
    public ResponseEntity getConfigurations(@RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {
        CronTasksConfigurationDto config = cronTaskConfigurationService.getTasksConfigurationDto();
        if (config == null || CollectionUtils.isEmpty(config.getCronTaskConfigurations()))
        {
            return getNotFoundResponseEntity(NOT_FOUND_CONFIGURATIONS, acceptHeader);
        }

        return ResponseEntity.ok(config);
    }

    @ApiOperation(value = "获取仓库已经设置的定时任务")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_CONFIGURATIONS),
            @ApiResponse(code = 404, message = NOT_FOUND_CONFIGURATIONS) })
    @GetMapping(value = "/getByRepository",produces = { MediaType.APPLICATION_JSON_VALUE,
            com.veadan.folib.net.MediaType.APPLICATION_YAML_VALUE })
    public ResponseEntity getConfigurationsByRepository(@RequestHeader(HttpHeaders.ACCEPT) String acceptHeader,
                                                        @RequestParam("storageId") String storageId,
                                                        @RequestParam("repositoryId") String repositoryId)

    {

        CronTasksConfigurationDto config = cronTaskConfigurationService.getTasksConfigurationDto();
        config.setCronTaskConfigurations(config.getCronTaskConfigurations().stream().filter(cron ->storageId.equals(cron.getProperty("storageId"))&&repositoryId.equals(cron.getProperty("repositoryId"))).collect(Collectors.toSet()));

        if (config == null || CollectionUtils.isEmpty(config.getCronTaskConfigurations()))
        {
            return ResponseEntity.ok(config);
        }

        return ResponseEntity.ok(config);
    }



    @ApiOperation(value = "Used to upload groovy script for groovy cron task")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_UPLOAD_GROOVY_SCRIPT),
                            @ApiResponse(code = 400, message = FAILED_UPLOAD_GROOVY_SCRIPT) })
    @PutMapping(value = "/cron/groovy/{UUID}",
                produces = { MediaType.TEXT_PLAIN_VALUE,
                             MediaType.APPLICATION_JSON_VALUE })
    public ResponseEntity uploadGroovyScript(@PathVariable("UUID") UUID uuid,
                                             HttpServletRequest request,
                                             @RequestHeader(HttpHeaders.ACCEPT) String acceptHeader)
    {

        final String fileName = request.getHeader(CRON_CONFIG_FILE_NAME_KEY);
        if (!fileName.endsWith(".groovy"))
        {
            return getBadRequestResponseEntity("The uploaded file must be a Groovy one!", acceptHeader);
        }

        CronTaskConfigurationDto cronTaskConfiguration = cronTaskConfigurationService.getTaskConfigurationDto(uuid);
        if (cronTaskConfiguration == null)
        {
            return getNotFoundResponseEntity(NOT_FOUND_CONFIGURATION, acceptHeader);
        }

        logger.info(">> CRON UUID: {}", cronTaskConfiguration.getUuid());
        logger.info(">> CRON NAME: {}", cronTaskConfiguration.getName());
        logger.info(">> Properties: {}", cronTaskConfiguration.getProperties());

        String path = propertiesBooter.getVaultDirectory() + "/etc/conf/cron/groovy";

        cronTaskConfiguration.addProperty(CRON_CONFIG_FILE_NAME_KEY, fileName);
        cronTaskConfiguration.setJobClass(GroovyCronJob.class.getName());
        cronTaskConfiguration.addProperty(CRON_CONFIG_SCRIPT_PATH_KEY, path + "/" + fileName);

        try
        {
            storeGroovyCronTask(request.getInputStream(), path, fileName);
            cronTaskConfigurationService.saveConfiguration(cronTaskConfiguration);
        }
        catch (Exception e)
        {
            return getExceptionResponseEntity(HttpStatus.INTERNAL_SERVER_ERROR, FAILED_UPLOAD_GROOVY_SCRIPT, e,
                                              acceptHeader);
        }

        return getSuccessfulResponseEntity(SUCCESSFUL_UPLOAD_GROOVY_SCRIPT, acceptHeader);
    }

    @ApiOperation(value = "Used to get all groovy scripts names")
    @ApiResponses(value = { @ApiResponse(code = 200, message = SUCCESSFUL_GET_GROOVY_SCRIPTS) })
    @GetMapping(value = "/groovy/names",
                produces = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity getGroovyScriptsName()
    {
        GroovyScriptNamesDto groovyScriptNames = cronJobSchedulerService.getGroovyScriptsName();

        return ResponseEntity.ok(groovyScriptNames);
    }

    private void storeGroovyCronTask(InputStream is,
                                     String dirPath,
                                     String fileName)
            throws IOException
    {
        Path dir = Paths.get(dirPath);

        if (!dir.toFile().exists())
        {
            Files.createDirectories(dir);
        }

        Path file = dir.resolve(fileName);
        Files.copy(is, file);
    }

}
