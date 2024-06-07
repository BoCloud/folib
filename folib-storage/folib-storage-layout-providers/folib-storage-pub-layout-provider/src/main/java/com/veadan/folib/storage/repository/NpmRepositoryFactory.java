//package com.veadan.folib.storage.repository;
//
//import com.veadan.folib.providers.layout.PubLayoutProvider;
//import com.veadan.folib.repository.NpmRepositoryFeatures;
//
//import javax.inject.Inject;
//import java.util.LinkedHashSet;
//
//import org.springframework.stereotype.Component;
//
///**
// * @author Veadan
// */
//@Component
//public class NpmRepositoryFactory
//        implements RepositoryFactory
//{
//
//    @Inject
//    private NpmRepositoryFeatures npmRepositoryFeatures;
//
//
//    @Override
//    public RepositoryDto createRepository(String repositoryId)
//    {
//        RepositoryDto repository = new RepositoryDto(repositoryId);
//        repository.setLayout(PubLayoutProvider.ALIAS);
//        repository.setArtifactCoordinateValidators(
//                new LinkedHashSet<>(npmRepositoryFeatures.getDefaultArtifactCoordinateValidators()));
//
//        return repository;
//    }
//
//}
