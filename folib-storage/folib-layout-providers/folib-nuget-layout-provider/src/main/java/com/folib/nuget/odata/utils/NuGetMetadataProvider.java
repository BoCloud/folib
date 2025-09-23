package com.folib.nuget.odata.utils;

import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.storage.repository.Repository;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;

import java.io.IOException;



@Slf4j
public class NuGetMetadataProvider {
    public static String getServiceDocumentEntity(Repository repository) {
        String doc = getResourceFile("/service_document.xml");
        return doc.replace("$$baseUrl$$", NuGetUrlBuilder.getNugetV2BaseUrl(repository));
    }

    public static String getMetadataTemplate(Repository repository) {
        return getResourceFile("/metadata.xml");
    }


    private static String getResourceFile(String path) {
        try {
            return IOUtils.toString(NuGetMetadataProvider.class.getResourceAsStream(path), "UTF-8");
        } catch (IOException e) {
            log.error("Unable to read resource: " + e.getMessage());
            log.debug("Unable to read resource: " + e.getMessage(), e);
            return "";
        }
    }
}
