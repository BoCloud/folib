package com.folib.repository;

import com.folib.nuget.odata.feed.Entry;
import com.folib.nugetv3.model.registration.RegistrationResult;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import com.folib.repositories.ArtifactRepository;
import com.folib.services.ArtifactIdGroupService;
import com.folib.services.ArtifactTagService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Repository;

import javax.inject.Inject;



@Slf4j
@Component
public class NugetPackageFeedParser {
    @Inject
    private ArtifactTagService artifactTagService;

    @Inject
    private ArtifactIdGroupService repositoryArtifactIdGroupService;

    @Inject
    private ArtifactRepository artifactRepository;

    // .nuGetV2/Entry/packageId/version.xml
    public void parseEntry(Repository repository, Entry entry) {

    }

    // .nuGetV3/registration/packageId/index.json
    // .nuGetV3/registration-semver2/packageId/index.json
    public void parseRegistrationResult(Repository repository, RegistrationResult registrationResult, String packageId, boolean shouldRemoveSemver2) {

    }

    // .nuGetV3/registration/packageId/version.json
    public void parseRegistrationResultPageItem(Repository repository, RegistrationResultPageItem registrationResultPageItem, String packageId, String version) {

    }
}
