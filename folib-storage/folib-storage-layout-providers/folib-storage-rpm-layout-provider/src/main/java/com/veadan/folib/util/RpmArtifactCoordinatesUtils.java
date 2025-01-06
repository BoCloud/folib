package com.veadan.folib.util;

import com.veadan.folib.artifact.coordinates.RpmArtifactCoordinates;
import org.apache.commons.io.FilenameUtils;
import com.veadan.folib.domain.RpmPackageArch;
import com.veadan.folib.domain.RpmPackageType;

import javax.validation.constraints.NotEmpty;


import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.veadan.folib.domain.RpmNamingPatterns.*;

/**
 * Util class for parsing generating artifact coordinates by provided RPM-packages.
 * @author Ilya Shatalov <ilya@alov.me>
 */
public class RpmArtifactCoordinatesUtils
{

    /**
     * The method parse RPM-package by provided path to file.
     * The type of package depends on package arch thar will be parsed.
     *
     * For example: libglvnd-1.1.0-4.gitf92208b.fc30.i686.rpm  -  where:
     * - libglvnd          - package base name;
     * - 1.1.0             - package version;
     * - 4.gitf92208b.fc30 - package release;
     * - i686              - package architecture;
     * - rpm               - package extension.
     * Package has I686 arch, so package type will be stamped as BINARY.
     *
     * @param path to RPM package on the filesystem;
     * @return compiled RpmArtifactCoordinates;
     */
    public static RpmArtifactCoordinates parse(@NotEmpty String path)
    {
        String rpmFileName = FilenameUtils.getName(path);
        if (!rpmFileName.endsWith(".rpm"))
        {
            throw new IllegalArgumentException("The artifact packaging can be only '.rpm'");
        }

        // 正则表达式分为四部分：baseName, version, release, packageType
        String regex = "^(.*?)-([^-]+)-([^-]+)\\.([^.]+)\\.rpm$";

        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(rpmFileName);

        if (matcher.matches()) {
            String baseName = matcher.group(1);
            String version = matcher.group(2);
            String release = matcher.group(3);
            String type = matcher.group(4);
            RpmArtifactCoordinates artifactCoordinates;
            if (RpmPackageType.SOURCE.getPostfix().equals(type))
            {
                artifactCoordinates = new RpmArtifactCoordinates(baseName, version, release,path,RpmPackageType.SOURCE);
            }
            else
            {
                RpmPackageArch arch = parseArch(rpmFileName);
                artifactCoordinates = new RpmArtifactCoordinates(baseName, version, release, RpmPackageType.valueOf(type.toUpperCase()), arch, path);
            }

            return artifactCoordinates;
        } else {
           throw new RuntimeException("The RPM filename does not match the expected format.");
        }
        //String baseName = parseBaseName(fileName);
        //String version = parseVersion(fileName);
        //String release = parseRelease(fileName);
        //RpmPackageType packageType = parsePackageType(fileName);



    }

    private static String parseBaseName(String fileName)
    {
        Matcher matcher = RPM_PACKAGE_NAME_REGEXP_PATTERN.matcher(fileName);

        if (!matcher.find())
        {
            throw new IllegalArgumentException("Incorrect filename: package name is required");
        }
        return matcher.group(1);
    }

    private static String parseVersion(String fileName)
    {
        Matcher matcher = RPM_PACKAGE_VERSION_REGEXP_PATTERN.matcher(fileName);

        if (!matcher.find())
        {
            throw new IllegalArgumentException("Incorrect filename: package version is required");
        }
        return matcher.group(1);
    }

    private static String parseRelease(String fileName)
    {
        String release;
        Matcher matcher =  RPM_PACKAGE_RELEASE_REGEXP_PATTERN.matcher(fileName);

        if (!matcher.find())
        {
            throw new IllegalArgumentException("Incorrect filename: package release is required");
        }
        release = matcher.group(1);

        return release;
    }

    private static RpmPackageType parsePackageType (String fileName)
    {
        boolean match = RPM_PACKAGE_TYPE_REGEXP_PATTERN.matcher(fileName).find();

        return match
                ? RpmPackageType.SOURCE
                : RpmPackageType.BINARY;
    }


    private static RpmPackageArch parseArch (String fileName)
    {
        String arch;
        Matcher matcher = RPM_PACKAGE_ARCH_REGEXP_PATTERN.matcher(fileName);

        if (!matcher.find())
        {
            throw new IllegalArgumentException("Incorrect filename: package should have architecture or SRC suffix");
        }
        arch = matcher.group(1);

        return RpmPackageArch.valueOf(arch.toUpperCase());
    }
}
