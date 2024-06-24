package com.veadan.folib.cron.jobs;

/**
 * @author Veadan
 */
public abstract class JavaCronJob
        extends AbstractCronJob
{

    public static final String GLOBAL="GLOBAL";
    public static final String MAVEN="MAVEN";
    public static final String NPM="NPM";
    public static final String RPM="RPM";
    public static final String DOCKER="DOCKER";
    public static final String NUGET="NUGET";
    public static final String COCOAPODS="COCOAPODS";
    public static final String CONAN="CONAN";
    public static final String PUB="PUB";

}
