package com.veadan.folib.domain.adapter.jfrog;

import com.veadan.folib.artifact.coordinates.CocoapodsArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.ConanArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.DockerArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.GitLfsArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.GoArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.HuggingFaceArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.MavenArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NpmArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.NugetArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PhpArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PubArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.PypiArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.RawArtifactCoordinates;
import com.veadan.folib.artifact.coordinates.RpmArtifactCoordinates;
import com.veadan.folib.constant.DebianConstant;
import com.veadan.folib.storage.repository.RepositoryDto;
import org.apache.commons.lang3.StringUtils;
import org.jfrog.artifactory.client.model.Privilege;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.jfrog.artifactory.client.model.Privilege.ADMIN;
import static org.jfrog.artifactory.client.model.Privilege.ANNOTATE;
import static org.jfrog.artifactory.client.model.Privilege.DELETE;
import static org.jfrog.artifactory.client.model.Privilege.DEPLOY;
import static org.jfrog.artifactory.client.model.Privilege.READ;

/**
 * @author huayanjun
 * @since 2024-10-24 10:24
 */
public class JfrogMapping {
    public static final Map<String, String> layoutMap = new HashMap<>();

    public static final String SUB_LAYOUT_GRADLE = "gradle";
    public static final String SUB_LAYOUT_MAVEN = "maven";
    public static final String SUB_LAYOUT_SBT = "sbt";
    public static final String SUB_LAYOUT_IVY = "ivy";

    public static final Set<String> MAVEN_SUB_LAYOUT = Set.of(SUB_LAYOUT_GRADLE, SUB_LAYOUT_MAVEN, SUB_LAYOUT_SBT, SUB_LAYOUT_IVY);

    static {
        layoutMap.put("Docker", DockerArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Npm", NpmArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Maven", SUB_LAYOUT_MAVEN);
        layoutMap.put("Pypi", PypiArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("HuggingFaceML", HuggingFaceArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Gradle", SUB_LAYOUT_GRADLE);
        layoutMap.put("Go", GoArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Debian", DebianConstant.LAYOUT_NAME);
        layoutMap.put("Rpm", RpmArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Yum",RpmArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Swift", "");
        layoutMap.put("Terraform", "");
        layoutMap.put("TerraformBE", "");
        layoutMap.put("Alpine", "");
        layoutMap.put("Bower", "");
        layoutMap.put("Cargo", "");
        layoutMap.put("Chef", "");
        layoutMap.put("CocoaPods", CocoapodsArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Conan", ConanArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Conda", "");
        layoutMap.put("CRAN", "");
        layoutMap.put("OCI", "");
        layoutMap.put("Gems", "");
        layoutMap.put("GitLfs", GitLfsArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Helm", "helm");
        layoutMap.put("Ivy", SUB_LAYOUT_IVY);
        layoutMap.put("NuGet", NugetArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Opkg", "");
        layoutMap.put("Composer", PhpArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Pub", PubArtifactCoordinates.LAYOUT_NAME);
        layoutMap.put("Puppet", "");
        layoutMap.put("SBT", SUB_LAYOUT_SBT);
        layoutMap.put("Vagrant", "");
        layoutMap.put("Generic", RawArtifactCoordinates.LAYOUT_NAME);
    }

    public static RepositoryDto initRepoByPackageType(String packageType) {
        RepositoryDto repository = new RepositoryDto();
        repository.setArtifactMaxSize(107374182400L);
        String subLayout = layoutMap.get(packageType);
        if (StringUtils.isBlank(subLayout)) {
            return null;
        }
        repository.setLayout(subLayout);
        if (MAVEN_SUB_LAYOUT.contains(subLayout)) {
            repository.setLayout(MavenArtifactCoordinates.LAYOUT_NAME);
        } else {
            repository.setLayout(subLayout);
        }
        return repository;
    }

    public static String accessConvert(Privilege privilege) {
        if (privilege == ADMIN) {
            return "ARTIFACTS_MANAGE";
        } else if (privilege == DELETE) {
            return "ARTIFACTS_DELETE";
        } else if (privilege == DEPLOY) {
            return "ARTIFACTS_DEPLOY";
        } else if (privilege == ANNOTATE) {
            return "CONFIGURATION_ADD_UPDATE_METADATA";
        } else if (privilege == READ) {
            return "ARTIFACTS_RESOLVE";
        }else {
            return "";
        }
    }


}
