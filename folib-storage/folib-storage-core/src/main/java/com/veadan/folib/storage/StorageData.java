package com.veadan.folib.storage;

import com.fasterxml.jackson.annotation.JsonView;
import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.ImmutableSortedMap;
import com.veadan.folib.json.MapValuesJsonSerializer;
import com.veadan.folib.storage.repository.Repository;
import com.veadan.folib.storage.repository.RepositoryData;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import javax.annotation.concurrent.Immutable;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import static java.util.stream.Collectors.toMap;

/**
 * @author veadan
 */
@Immutable
@XmlAccessorType(XmlAccessType.FIELD)
@SuppressFBWarnings(value = "AJCIP_FIELD_ISNT_FINAL_IN_IMMUTABLE_CLASS")
public class StorageData implements Storage {

    @JsonView(Views.ShortStorage.class)
    private String id;

    @JsonView(Views.ShortStorage.class)
    private String basedir;

    @JsonView(Views.ShortStorage.class)
    private String admin;

    @JsonView(Views.ShortStorage.class)
    private Set<String> users;

    @JsonView(Views.LongStorage.class)
    @JsonSerialize(using = MapValuesJsonSerializer.class)
    @JsonDeserialize(using = RepositoryArrayToMapJsonDeserializer.class)
    private Map<String, ? extends Repository> repositories;

    StorageData() {

    }

    public StorageData(final Storage delegate) {
        this.id = delegate.getId();
        this.basedir = delegate.getBasedir();
        this.admin = delegate.getAdmin();
        this.users = delegate.getUsers();
        this.repositories = immuteRepositories(delegate.getRepositories());
    }

    private Map<String, ? extends Repository> immuteRepositories(final Map<String, ? extends Repository> source) {
        return source != null ? ImmutableSortedMap.copyOf(source.entrySet().stream().collect(
                toMap(Map.Entry::getKey, e -> new RepositoryData(e.getValue(), this)))) : Collections.emptyMap();
    }

    @Override
    public Repository getRepository(final String repositoryId) {
        return repositories.get(repositoryId);
    }

    @Override
    public boolean containsRepository(final String repository) {
        return repositories.containsKey(repository);
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public String getAdmin() {
        return admin;
    }

    @Override
    public String getBasedir() {
        return basedir;
    }

    @Override
    public Set<String> getUsers() {
        return users;
    }

    @Override
    public Map<String, ? extends Repository> getRepositories() {
        return repositories;
    }
}
