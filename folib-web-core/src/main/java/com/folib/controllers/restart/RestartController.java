package com.folib.controllers.restart;

import com.folib.app.FolibSpringBootApplication;
import com.folib.controllers.BaseController;
import io.swagger.annotations.Api;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static com.folib.controllers.restart.RestartController.MAPPING;


/**
 * @author: adavid9
 */
@RestController
@PreAuthorize("hasAuthority('ADMIN')")
@RequestMapping(value = MAPPING)
@Api(description = "服务重启控制器",tags = "服务重启控制器")
public class RestartController
        extends BaseController
{

    public static final String MAPPING = "/api";

    private static final Logger logger = LoggerFactory.getLogger(RestartController.class);

    @PostMapping("/restart")
    public void restart()
    {
        FolibSpringBootApplication.restart();
        logger.info("Restarting folib application.");
    }
}
