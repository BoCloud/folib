//package com.veadan.folib.util;
//
//import com.veadan.folib.domain.Artifact;
//import com.veadan.folib.domain.DebianMetadata;
//
///**
// * @author huayanjun
// * @since 2024-09-04 14:04
// */
//public class DebianPropertyWriter {
//
//    public void writeArtifactAttributes(Artifact artifact, DebianMetadata metadata) {
//        RepoPath path = DpkgUtils.artifactPath(artifact);
//
//        PropertiesService propertiesService = (PropertiesService)ContextHelper.get().beanForType(PropertiesService.class);
//        Properties existingProps = propertiesService.getProperties(path);
//        Properties propsToWrite = (Properties)InfoFactoryHolder.get().createProperties();
//        this.populatePackageTypeSpecificProperties(propsToWrite, metadata);
//        if (!propsToWrite.isEmpty()) {
//            if (log.isDebugEnabled()) {
//                log.debug("Setting metadata as properties on path {}", path.toPath());
//            }
//
//            this.preserveExistingProperties(existingProps, propsToWrite, path);
//            propertiesService.setProperties(path, propsToWrite, true);
//            if (artifact instanceof ArtifactoryArtifact) {
//                ArtifactoryArtifact artifactoryArtifact = (ArtifactoryArtifact)artifact;
//                if (log.isDebugEnabled()) {
//                    log.debug("Indexing DPKG package at {} to Metadata Service", path.toPath());
//                }
//
//                ((MetadataServiceIndexHandler)ContextHelper.get().beanForType(MetadataServiceIndexHandler.class)).indexPackageAsync(artifactoryArtifact.getFileInfo(), propsToWrite);
//            }
//        } else if (log.isDebugEnabled()) {
//            log.debug("No properties to write on path {}, skipping", path.toPath());
//        }
//
//        AddonsManager addonsManager = (AddonsManager)ContextHelper.get().beanForType(AddonsManager.class);
//        if (StringUtils.isNotBlank(metadata.license)) {
//            ((LicensesAddon)addonsManager.addonByType(LicensesAddon.class)).setLicensePropsOnPath(path, new String[]{metadata.license});
//        }
//
//    }
//}
