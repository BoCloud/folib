package com.folib.actuator;

import com.folib.booters.PropertiesBooter;
import org.springframework.boot.actuate.info.Info;
import org.springframework.boot.actuate.info.InfoContributor;
import org.springframework.stereotype.Component;

import javax.inject.Inject;
import java.util.HashMap;
import java.util.Map;

/**
 * @author: adavid9
 */
@Component(value = "extendedInfoEndpoint")
public class FolibCustomInfo implements InfoContributor
{

    @Inject
    private PropertiesBooter propertiesBooter;

    @Override
    public void contribute(Info.Builder builder)
    {
        Map<String, String> folibInfo = new HashMap<>();
        folibInfo.put("version", propertiesBooter.getFolibVersion());
        folibInfo.put("revision", propertiesBooter.getFolibRevision());
        builder.withDetail("folib", folibInfo);
    }
}
