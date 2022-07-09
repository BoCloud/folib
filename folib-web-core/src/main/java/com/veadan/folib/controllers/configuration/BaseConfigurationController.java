package com.veadan.folib.controllers.configuration;

import com.veadan.folib.controllers.BaseController;
import com.veadan.folib.services.ConfigurationManagementService;

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
