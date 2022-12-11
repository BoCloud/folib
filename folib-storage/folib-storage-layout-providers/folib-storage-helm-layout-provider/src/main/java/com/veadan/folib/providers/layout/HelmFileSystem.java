package com.veadan.folib.providers.layout;

import com.veadan.folib.booters.PropertiesBooter;
import com.veadan.folib.providers.io.LayoutFileSystem;
import com.veadan.folib.storage.repository.Repository;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;

import java.nio.file.FileSystem;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class HelmFileSystem extends LayoutFileSystem {

    public HelmFileSystem(PropertiesBooter propertiesBooter,
                          Repository repository,
                          FileSystem storageFileSystem,
                          LayoutFileSystemProvider provider) {
        super(propertiesBooter, repository, storageFileSystem, provider);
    }

    @Override
    public Set<String> getDigestAlgorithmSet() {
        return Stream.of(MessageDigestAlgorithms.MD5, MessageDigestAlgorithms.SHA_1)
                .collect(Collectors.toSet());
    }
}
