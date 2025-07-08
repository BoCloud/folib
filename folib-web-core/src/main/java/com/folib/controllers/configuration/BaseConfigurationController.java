package com.folib.controllers.configuration;

import com.folib.controllers.BaseController;
import com.folib.services.ConfigurationManagementService;

/**
 * @author Veadan
 */
public abstract class BaseConfigurationController
        extends BaseController
{

    protected final ConfigurationManagementService configurationManagementService;


    protected BaseConfigurationController(ConfigurationManagementService configurationManagementService)
    {
        this.configurationManagementService = configurationManagementService;
    }

}
