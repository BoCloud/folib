package com.folib.nugetv3.registration;

import com.folib.nuget.indexer.model.NuSpecDependency;
import com.folib.nuget.indexer.model.NuSpecDependencyGroup;
import com.folib.nuget.indexer.model.NugetMetadata;
import com.folib.nuget.utils.NuGetUrlBuilder;
import com.folib.nuget.utils.NugetVersionUtil;
import com.folib.nugetv3.model.dependency.Dependency;
import com.folib.nugetv3.model.dependency.DependencyGroupsItem;
import com.folib.nugetv3.model.registration.RegistrationResult;
import com.folib.nugetv3.model.registration.RegistrationResultCatalogEntry;
import com.folib.nugetv3.model.registration.RegistrationResultPage;
import com.folib.nugetv3.model.registration.RegistrationResultPageItem;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * @author LingengMa
 * @date 2025/05/15 14:13
 * @Description: 注册索引和注册页
 */


@Slf4j
public class NugetV3RegistrationUtil {
    public static RegistrationResult registrationResultPagesToRegistrationResult(@NonNull List<RegistrationResultPage> pages, String v3RegistrationBaseUrl) {
        if (pages.isEmpty()) {
            throw new IllegalArgumentException("Cannot build registration with no package version");
        }
        List<RegistrationResultPageItem> pageItems = new ArrayList<>();
        for (RegistrationResultPage page : pages) {
            if (page.getItems() != null && !page.getItems().isEmpty()) {
                pageItems.addAll(page.getItems());
            }
        }
        return registrationResultPageItemsToRegistrationResult(pageItems, v3RegistrationBaseUrl);
    }

    public static RegistrationResult registrationResultPageItemsToRegistrationResult(@NonNull List<RegistrationResultPageItem> pageItems, String v3RegistrationBaseUrl) {
        if (pageItems.isEmpty()) {
            throw new IllegalArgumentException("Cannot build registration with no package version");
        } else {
            int versionCount = pageItems.size();
            int pagesCount = getNumberOfPages(versionCount);
            String packageId = pageItems.get(0).getCatalogEntry().getPackageId();
            List<RegistrationResultPage> pages = rebuildRegistrationResultItems(pageItems, v3RegistrationBaseUrl, versionCount, pagesCount, packageId);
            return RegistrationResult.builder().id(NuGetUrlBuilder.registration(v3RegistrationBaseUrl, packageId)).count(pagesCount).items(pages).build();
        }
    }


    public static RegistrationResultPage registrationResultPageItemsToRegistrationResultPage(List<RegistrationResultPageItem> pageItems, String packageId, String lower, String upper, String v3RegistrationBaseUrl) {
        String pageUrl = NuGetUrlBuilder.registrationPage(v3RegistrationBaseUrl, packageId, lower, upper);
        String registrationUrl = NuGetUrlBuilder.registration(v3RegistrationBaseUrl, packageId);
        List<RegistrationResultPageItem> items = pageItems.stream().filter((i) -> NugetVersionUtil.between(lower, upper, i.getCatalogEntry().getVersion())).collect(Collectors.toList());
        return RegistrationResultPage.builder().id(pageUrl).count(items.size()).lower(lower).upper(upper).parent(registrationUrl).items(items).build();
    }


    public static RegistrationResultPageItem metadataToRegistrationResultPageItem(NugetMetadata metadata, String v3RegistrationBaseUrl, String v2BaseUrl) {
        String id = metadata.getId();
        String packageContent = v2BaseUrl == null ? null : NuGetUrlBuilder.packageContent(v2BaseUrl, id, metadata.getVersion());
        List<DependencyGroupsItem> dependencyGroupsItems = metadataDependenciesToDependencyGroupsItems(v3RegistrationBaseUrl, metadata.getDependencies());
        RegistrationResultCatalogEntry catalogEntry = metadataToRegistrationResultCatalogEntry(metadata, dependencyGroupsItems, v2BaseUrl);
        String registrationPageUrl = NuGetUrlBuilder.registrationSpecificVersion(v3RegistrationBaseUrl, id, catalogEntry.getVersion());
        return RegistrationResultPageItem.builder().id(registrationPageUrl).catalogEntry(catalogEntry).packageContent(packageContent).registration(NuGetUrlBuilder.registration(v3RegistrationBaseUrl, id)).build();


    }

    private static List<DependencyGroupsItem> metadataDependenciesToDependencyGroupsItems(String v3RegistrationBaseUrl, List dependencies) {
        List<DependencyGroupsItem> dependencyGroupsItems = new ArrayList();
        if (dependencies != null && !dependencies.isEmpty()) {
            for (Object dependency : dependencies) {
                if (dependency instanceof NuSpecDependency) {
                    DependencyGroupsItem dependencyGroupsItem = nuSpecDependencyToDependencyGroupsItem((NuSpecDependency) dependency, v3RegistrationBaseUrl);
                    dependencyGroupsItems.add(dependencyGroupsItem);
                } else if (dependency instanceof NuSpecDependencyGroup) {
                    DependencyGroupsItem dependencyGroupsItem = nuSpecDependencyGroupToDependencyGroupsItem((NuSpecDependencyGroup) dependency, v3RegistrationBaseUrl);
                    dependencyGroupsItems.add(dependencyGroupsItem);
                } else {
                    String message = "Unsupported dependency type: " + dependency.getClass().getName();
                    log.debug(message);
                    throw new IllegalArgumentException(message);
                }
            }
        }
        return dependencyGroupsItems;
    }

    private static Dependency nuspecDependencyToDependency(String v3RegistrationBaseUrl, NuSpecDependency nuSpecDependency) {
        String packageId = nuSpecDependency.getId();
        String version = nuSpecDependency.getVersion();
        String registrationUrl = NuGetUrlBuilder.registrationSpecificVersion(v3RegistrationBaseUrl, packageId, version);
        return Dependency.builder().packageId(packageId).range(version).registration(registrationUrl).build();
    }

    private static DependencyGroupsItem nuSpecDependencyToDependencyGroupsItem(NuSpecDependency nuSpecDependency, String v3RegistrationBaseUrl) {
        ArrayList<Dependency> dependencies = new ArrayList();
        dependencies.add(nuspecDependencyToDependency(v3RegistrationBaseUrl, nuSpecDependency));
        return DependencyGroupsItem.builder().targetFramework("").dependencies(dependencies).build();
    }

    private static DependencyGroupsItem nuSpecDependencyGroupToDependencyGroupsItem(NuSpecDependencyGroup nuSpecDependencyGroup, String v3RegistrationBaseUrl) {
        String targetFramework = nuSpecDependencyGroup.getTargetFramework();
        ArrayList<Dependency> dependencies = new ArrayList();
        List<NuSpecDependency> nuSpecDependencies = nuSpecDependencyGroup.getDependencies();
        if (nuSpecDependencies != null && !nuSpecDependencies.isEmpty()) {
            for (NuSpecDependency nuSpecDependency : nuSpecDependencies) {
                dependencies.add(nuspecDependencyToDependency(v3RegistrationBaseUrl, nuSpecDependency));
            }
        }
        return DependencyGroupsItem.builder().targetFramework(targetFramework).dependencies(dependencies).build();
    }

    private static RegistrationResultCatalogEntry metadataToRegistrationResultCatalogEntry(NugetMetadata metadata, List<DependencyGroupsItem> dependencyGroupsItems, String v2BaseUrl) {
        String id = metadata.getId();
        String packageContent = v2BaseUrl == null ? null : NuGetUrlBuilder.packageContent(v2BaseUrl, id, metadata.getVersion());
        return RegistrationResultCatalogEntry
                .builder()
                .authors(metadata.getAuthors())
                .description(metadata.getDescription())
                .iconUrl(metadata.getIconUrl() == null ? null : URI.create(metadata.getIconUrl()))
                .packageId(id)
                .language(metadata.getLanguage())
                .licenseUrl(metadata.getLicenseUrl())
                .listed(true)
                .packageContent(packageContent)
                .projectUrl(metadata.getProjectUrl())
                .requireLicenseAcceptance(metadata.isRequireLicenseAcceptance())
                .summary(metadata.getSummary())
                .tags(metadata.getTags() == null ? null : Arrays.asList(metadata.getTags().split(" ")))
                .title(metadata.getTitle())
                .version(metadata.getVersion())
                .dependencyGroups(dependencyGroupsItems.isEmpty() ? null : dependencyGroupsItems)
                .build();
    }


    private static List<RegistrationResultPage> rebuildRegistrationResultItems(List<RegistrationResultPageItem> pageItems, String v3RegistrationBaseUrl, int versionCount, int pagesCount, String packageId) {
        return IntStream.range(0, pagesCount).mapToObj((i) -> {
            String lowerVersion = trimSemVer2MetaData((pageItems.get(64 * i)).getCatalogEntry().getVersion());
            boolean isLastPage = i == pagesCount - 1;
            int lastPackageIndexInPage = isLastPage ? versionCount - 1 : 64 * i + 63;
            String upperVersion = trimSemVer2MetaData((pageItems.get(lastPackageIndexInPage)).getCatalogEntry().getVersion());
            int packagesInPageCount = getPackagesInPageCount(versionCount, isLastPage);
            List<RegistrationResultPageItem> items = pageItems.subList(64 * i, 64 * i + packagesInPageCount);
            return RegistrationResultPage.builder()
                    .id(NuGetUrlBuilder.registrationPage(v3RegistrationBaseUrl, packageId, lowerVersion, upperVersion))
                    .count(packagesInPageCount)
                    .lower(lowerVersion)
                    .upper(upperVersion)
                    .items(items)
                    .build();
        }).collect(Collectors.toList());
    }

    public static RegistrationResult merge(List<RegistrationResult> registrationResults) {
        if (registrationResults == null || registrationResults.isEmpty()) {
            return null;
        }

        List<RegistrationResultPage> mergedPages = new ArrayList<>();
        for (RegistrationResult result : registrationResults) {
            if (result != null && result.getItems() != null) {
                mergedPages.addAll(result.getItems());
            }
        }

        if (mergedPages.isEmpty()) {
            return null;
        }

        String packageId = mergedPages.get(0).getItems().get(0).getCatalogEntry().getPackageId();
        String v3RegistrationBaseUrl = NuGetUrlBuilder.registration(mergedPages.get(0).getId(), packageId);
        return RegistrationResult.builder()
                .id(NuGetUrlBuilder.registration(v3RegistrationBaseUrl, packageId))
                .count(mergedPages.size())
                .items(mergedPages)
                .build();
    }

    private static int getPackagesInPageCount(int totalPackageCount, boolean lastPage) {
        if (totalPackageCount < 64 && !lastPage) {
            String message = "Number of packages in a page must be 64 unless it's the last page";
            log.debug(message);
            throw new IllegalArgumentException(message);
        } else {
            int packagesInPageCount = 64;
            if (lastPage && totalPackageCount % 64 != 0) {
                packagesInPageCount = totalPackageCount % 64;
            }

            return packagesInPageCount;
        }
    }

    public static String trimSemVer2MetaData(String version) {
        if (version == null) {
            return null;
        } else {
            int endIndex = version.indexOf(43);
            return endIndex == -1 ? version : version.substring(0, endIndex);
        }
    }

    private static int getNumberOfPages(int packageCount) {
        return packageCount / 64 + (packageCount % 64 != 0 ? 1 : 0);
    }
}
