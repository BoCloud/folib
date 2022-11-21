package com.veadan.folib.config;

import org.glassfish.jersey.media.multipart.MultiPartFeature;
import org.glassfish.jersey.server.ResourceConfig;

import javax.ws.rs.ApplicationPath;

@ApplicationPath("webapi")
public class RestApplication extends ResourceConfig {
    public RestApplication() {
        this.packages("cn.wolfcode.jersey");
        this.register(MultiPartFeature.class);
    }
}


