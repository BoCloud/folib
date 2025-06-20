//package com.veadan.folib.util;
//
//import com.veadan.folib.artifact.coordinates.HelmArtifactCoordinates;
//import org.apache.commons.io.FilenameUtils;
//
//public class HelmArtifactCoordinatesUtils {
////    public static HelmArtifactCoordinates parse(String path) {
////        String fileName = FilenameUtils.getName(path);
////        String baseName = fileName; //parseBaseName(fileName);
////        String version = parseVersion(fileName);
////        String release = "";
////        HelmPackageType packageType = parsePackageType(path);
////        HelmArtifactCoordinates artifactCoordinates;
////        if (packageType == HelmPackageType.SOURCE) {
////            artifactCoordinates = new HelmArtifactCoordinates(baseName, version, release, packageType);
////        } else {
////            HelmPackageArch arch = parseArch(path);
////            artifactCoordinates = new HelmArtifactCoordinates(baseName, version, release, packageType, arch);
////        }
////        return artifactCoordinates;
////    }
//
//    private static HelmPackageArch parseArch(String path) {
//        return path.endsWith("tgz") ? HelmPackageArch.K8S : HelmPackageArch.NOARCH;
//    }
//
//    private static HelmPackageType parsePackageType(String path) {
//        if (path.endsWith(".tgz")) {
//            return HelmPackageType.CHART;
//        } else {
//            return HelmPackageType.SOURCE;
//        }
//    }
//
//    private static String parseVersion(String fileName) {
//        if (fileName.endsWith(".tgz")) {
//            String[] array = fileName.split("-");
//            return array.length > 0 ? array[1].replace(".tgz", "") : "";
//        } else {
//            return "";
//        }
//    }
//
//    private static String parseBaseName(String fileName) {
//        return fileName.split("-")[0];
//    }
//}
