package com.veadan.folib.controllers.configuration;

import com.veadan.folib.controllers.BaseController;
import io.swagger.annotations.Api;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * @author Veadan
 */
@Controller
@RequestMapping("/api/systemConfiguration")
@Api(description = "系统设置", tags = "系统设置")
public class SystemConfigurationController
        extends BaseController {

}
