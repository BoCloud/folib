package com.veadan.folib.artifact.coordinates;

import com.veadan.folib.providers.io.RepositoryPath;
import lombok.Data;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.handler.ArtifactHandler;
import org.apache.maven.artifact.metadata.ArtifactMetadata;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.resolver.filter.ArtifactFilter;
import org.apache.maven.artifact.versioning.ArtifactVersion;
import org.apache.maven.artifact.versioning.OverConstrainedVersionException;
import org.apache.maven.artifact.versioning.VersionRange;
import org.jetbrains.annotations.NotNull;
import java.io.File;
import java.util.Collection;
import java.util.List;

@Data
public class HelmRepositoryArtifact implements HelmArtifact{
    private String baseName;
    private String packageType;
    private String metaData;
    private String artifactSuffix;

    public HelmRepositoryArtifact() {
    }

    public HelmRepositoryArtifact(String baseName, String packageType, String metaData, String artifactSuffix) {
        this.baseName = baseName;
        this.packageType = packageType;
        this.metaData = metaData;
        this.artifactSuffix = artifactSuffix;
    }

    @Override
    public RepositoryPath getPath() {
        return null;
    }

    @Override
    public void setPath(RepositoryPath destination) {

    }

    @Override
    public String getGroupId() {
        return null;
    }

    @Override
    public String getArtifactId() {
        return null;
    }

    @Override
    public String getVersion() {
        return null;
    }

    @Override
    public void setVersion(String s) {

    }

    @Override
    public String getScope() {
        return null;
    }

    @Override
    public String getType() {
        return null;
    }

    @Override
    public String getClassifier() {
        return null;
    }

    @Override
    public boolean hasClassifier() {
        return false;
    }

    @Override
    public File getFile() {
        return null;
    }

    @Override
    public void setFile(File file) {

    }

    @Override
    public String getBaseVersion() {
        return null;
    }

    @Override
    public void setBaseVersion(String s) {

    }

    @Override
    public String getId() {
        return null;
    }

    @Override
    public String getDependencyConflictId() {
        return null;
    }

    @Override
    public void addMetadata(ArtifactMetadata artifactMetadata) {

    }

    @Override
    public Collection<ArtifactMetadata> getMetadataList() {
        return null;
    }

    @Override
    public void setRepository(ArtifactRepository artifactRepository) {

    }

    @Override
    public ArtifactRepository getRepository() {
        return null;
    }

    @Override
    public void updateVersion(String s, ArtifactRepository artifactRepository) {

    }

    @Override
    public String getDownloadUrl() {
        return null;
    }

    @Override
    public void setDownloadUrl(String s) {

    }

    @Override
    public ArtifactFilter getDependencyFilter() {
        return null;
    }

    @Override
    public void setDependencyFilter(ArtifactFilter artifactFilter) {

    }

    @Override
    public ArtifactHandler getArtifactHandler() {
        return null;
    }

    @Override
    public List<String> getDependencyTrail() {
        return null;
    }

    @Override
    public void setDependencyTrail(List<String> list) {

    }

    @Override
    public void setScope(String s) {

    }

    @Override
    public VersionRange getVersionRange() {
        return null;
    }

    @Override
    public void setVersionRange(VersionRange versionRange) {

    }

    @Override
    public void selectVersion(String s) {

    }

    @Override
    public void setGroupId(String s) {

    }

    @Override
    public void setArtifactId(String s) {

    }

    @Override
    public boolean isSnapshot() {
        return false;
    }

    @Override
    public void setResolved(boolean b) {

    }

    @Override
    public boolean isResolved() {
        return false;
    }

    @Override
    public void setResolvedVersion(String s) {

    }

    @Override
    public void setArtifactHandler(ArtifactHandler artifactHandler) {

    }

    @Override
    public boolean isRelease() {
        return false;
    }

    @Override
    public void setRelease(boolean b) {

    }

    @Override
    public List<ArtifactVersion> getAvailableVersions() {
        return null;
    }

    @Override
    public void setAvailableVersions(List<ArtifactVersion> list) {

    }

    @Override
    public boolean isOptional() {
        return false;
    }

    @Override
    public void setOptional(boolean b) {

    }

    @Override
    public ArtifactVersion getSelectedVersion() throws OverConstrainedVersionException {
        return null;
    }

    @Override
    public boolean isSelectedVersionKnown() throws OverConstrainedVersionException {
        return false;
    }

    @Override
    public int compareTo(@NotNull Artifact o) {
        return 0;
    }
}
