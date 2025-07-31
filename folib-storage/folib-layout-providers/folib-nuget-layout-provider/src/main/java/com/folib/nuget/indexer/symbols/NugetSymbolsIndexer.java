package com.folib.nuget.indexer.symbols;


import com.folib.nuget.indexer.symbols.pdb.PdbGuid;
import com.folib.nuget.indexer.symbols.pdb.PdbParser;
import com.folib.nuget.indexer.symbols.pdb.PdbParserFactory;
import com.folib.providers.io.RepositoryPath;
import com.folib.providers.io.RepositoryPathResolver;
import com.folib.storage.repository.Repository;
import com.folib.utils.PathUtils;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

@Slf4j
public class NugetSymbolsIndexer {
    private final RepositoryPathResolver repositoryPathResolver;

    public NugetSymbolsIndexer(RepositoryPathResolver repositoryPathResolver) {
        this.repositoryPathResolver = repositoryPathResolver;
    }


    public void indexSymbolArtifact(RepositoryPath artifactPath) {
        if (artifactPath == null || !Files.exists(artifactPath)) {
            log.error("Artifact path is null or does not exist");
            throw new IllegalArgumentException("Artifact path is null or does not exist");
        }

        String symbolArtifactName = artifactPath.getFileName().toString();
        Repository repository = artifactPath.getRepository();

        try (InputStream stream = Files.newInputStream(artifactPath)) {
            try (ZipInputStream nuPkgZipStream = new ZipInputStream(stream)) {
                ZipEntry entry;
                while ((entry = nuPkgZipStream.getNextEntry()) != null) {
                    String entryName = entry.getName();
                    if (this.isSymbolFile(entryName)) {
                        log.debug("Found symbol file in {}", entryName);
                        byte[] fileBytes = this.getAsByteArray(nuPkgZipStream);
                        String guidString = this.parseAndRetrieveGuidFromPdbFile(entryName, fileBytes);
                        if (guidString != null && !guidString.isEmpty()) {
                            this.uploadPdbToSymbolsDir(repository, fileBytes, guidString, entryName);
                        } else {
                            log.debug("GUID is empty for symbol file '{}', symbol will not be indexed", entryName);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("Error reading artifact path", e);
            throw new RuntimeException("Error reading artifact path", e);
        }
    }

    private boolean isSymbolFile(String filename) {
        return filename != null && filename.endsWith(".pdb");
    }

    private byte[] getAsByteArray(final ZipInputStream in) throws IOException {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        IOUtils.copy(in, out);
        return out.toByteArray();
    }

    private String parseAndRetrieveGuidFromPdbFile(String entryName, byte[] fileBytes) {
        try {
            PdbParser pdbParser = PdbParserFactory.create(fileBytes);
            PdbGuid guid = pdbParser.parsePdbAndExtractGuid();
            if (guid != null) {
                return guid.getGuidAsString();
            }
        } catch (Exception e) {
            log.error("Could not parse symbol file '{}' because of the following: {}", entryName, e.getMessage());
            log.debug("Could not parse symbol file '{}' because of the following: ", entryName, e);
        }

        return null;
    }

    private void uploadPdbToSymbolsDir(Repository repository, byte[] fileBytes, String guid, String entryName) {
        String filename = PathUtils.getLastPathElement(entryName);
        String packageId = PathUtils.stripExtension(filename);
        String pathInCache = getSymbolFilePathInCache(packageId, guid);
        RepositoryPath repositoryPath = repositoryPathResolver.resolve(repository, pathInCache);
        try (InputStream is = new ByteArrayInputStream(fileBytes)) {
            Files.createDirectories(repositoryPath.getParent());
            if (Files.exists(repositoryPath)) {
                log.debug("File already exists in .symbols, skipping upload: {}", repositoryPath);
                return;
            }
            Files.copy(is, repositoryPath);
        } catch (Exception e) {
            log.error("Could not upload pdb file to .symbols because of the following: {}", e.getMessage());
            log.debug("Could not upload pdb file to .symbols because of the following: ", e);
        }
    }

    public static String getSymbolFilePathInCache(@NonNull String packageId, @NonNull String guidString) {
        return String.format(".symbols/%s/%s/%s.pdb", packageId, guidString, packageId).toLowerCase();
    }


}
