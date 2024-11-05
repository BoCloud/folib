package com.veadan.folib.utils;

import cn.hutool.core.util.StrUtil;
import com.veadan.folib.model.request.MlModelRequestContext;
import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class MlModelRemoteUtils {

    private static final Logger log = LoggerFactory.getLogger(MlModelRemoteUtils.class);

    private static final String REVISION = "revision";

    private static final String RESOLVE = "resolve";

    private static final String HUGGING_FACE_WEBSITE_API = "api/models";


    private MlModelRemoteUtils() {
        throw new UnsupportedOperationException("This is a utility class and cannot be instantiated");
    }

    
    public static String getHuggingFaceAlternativeFileUrl(MlModelRequestContext context, String  remoteBaseUrl) {
        String alternativeUrl;
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (remoteBaseUrl == null) {
            throw new NullPointerException("remoteBaseUrl is marked non-null but is null");
        }
        if (StrUtil.isBlank(context.getOrg())) {
            alternativeUrl = String.join("/", getMlModelRemoteBaseUrl(context, remoteBaseUrl), context.getModelName(), "resolve", context
                    .getOriginalRemoteCommit(), context.getFile());
        } else {
            alternativeUrl = String.join("/",  getMlModelRemoteBaseUrl(context, remoteBaseUrl), context.getOrg(), context
                    .getModelName(), "resolve", context.getOriginalRemoteCommit(), context.getFile() );
        }
        try {
            return URIUtils.encodeQuery(alternativeUrl);
        } catch (HttpException  e) {
            log.warn("Could not encode path '{}', returning the un-escaped value.", alternativeUrl);
            return alternativeUrl;
        }
    }

    public static String getModelInfoAlternativeUrl( String organization,  String model,  String revision, String remoteBaseUrl ) {
        if (model == null) {
            throw new NullPointerException("model is marked non-null but is null");
        }
        if (revision == null) {
            throw new NullPointerException("revision is marked non-null but is null");
        }
        if (remoteBaseUrl == null) {
            throw new NullPointerException("remoteBaseUrl is marked non-null but is null");
        }
        if (StrUtil.isBlank(organization)) {
            return String.join("/",  remoteBaseUrl, "api/models", model, "revision", revision);
        }
        return String.join("/",   remoteBaseUrl, "api/models", organization, model, "revision", revision );
    }

    public static String getMlModelRemoteBaseUrl( String repoKey,  String remoteBaseUrl ) {
        if (repoKey == null) {
            throw new NullPointerException("repoKey is marked non-null but is null");
        }
        if (remoteBaseUrl == null) {
            throw new NullPointerException("configService is marked non-null but is null");
        }
        return remoteBaseUrl;//configService.getStringValue(repoKey, PackageDescriptorKey.URL, "");
    }

    public static String getMlModelRemoteBaseUrl( MlModelRequestContext context,  String remoteBaseUrl ) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        if (remoteBaseUrl == null) {
            throw new NullPointerException("configService is marked non-null but is null");
        }
        return remoteBaseUrl;//configService.getStringValue(context.getRepoKey(), PackageDescriptorKey.URL, "");
    }

    public static String getLatestModelInfoPath(MlModelRequestContext context) {
        if (context == null) {
            throw new NullPointerException("context is marked non-null but is null");
        }
        return String.join("/", getModelPath(context.getOrg(), context.getModelName()), "main", ".latest_huggingface_model_info.json");
    }

    private static String getModelPath(String organization, String modelName) {
        if (modelName == null) {
            throw new NullPointerException("modelName is marked non-null but is null");
        }
        if (StringUtils.isBlank(organization)) {
            return String.join("/", "models", modelName);
        }
        return String.join("/", "models", organization, modelName);
    }

    public static String getFilePath(String organization, String model,  String timestamp, String fileName) {
        if (model == null) {
            throw new NullPointerException("model is marked non-null but is null");
        }
        if (timestamp == null) {
            throw new NullPointerException("timestamp is marked non-null but is null");
        }
        if (fileName == null) {
            throw new NullPointerException("fileName is marked non-null but is null");
        }
        return String.join("/", getModelPath(organization, model), "main", timestamp, fileName);
    }
}

