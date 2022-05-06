package com.veadan.folib.users.userdetails;

import com.veadan.folib.domain.User;
import org.springframework.security.core.userdetails.UserDetails;

/**
 * This interface purpose is to manage folib external users cache within
 * authentication components.<br>
 * Extarnal users stored near with regular users but the difference is that
 * external users have not empty value of {@link User#getSourceId()}, which
 * pointing to external system ID where the external user come from.<br>
 * Another thing specific for external users is that they have configurable
 * cache invalidation time, so when it expires then the external user should be
 * invalidated and deleted. So the value of {@link User#getLastUpdated()} updated
 * every time
 * {@link FolibExternalUsersCacheManager#cacheExternalUserDetails(String, UserDetails)}
 * method called, this extends the period during which the external user remains
 * valid.
 * 
 * @author sbespalov
 */
public interface FolibExternalUsersCacheManager
{

    /**
     * Searches the user within folib users storage. The user can be
     * internal folib user, or external user chached in users storage. <br>
     * 
     * @param username
     *            the username to search
     * 
     * @return the {@link User} instance found or null
     */
    User findByUsername(String username);

    /**
     * Updates or creates external users which is chaced in folib users
     * storage.<br>
     * This method call also updates the {@link User#getLastUpdated()} value.
     * 
     * @param externalSourceId
     *            the identifier of external users provider
     * @param user
     *            the {@link UserDetails} to be used to update the cached
     *            {@link User}
     * @return the cached {@link User} instance
     */
    User cacheExternalUserDetails(String externalSourceId,
                                  UserDetails user);

    /**
     * Deletes stored {@link User} instance from folib users storage. This
     * is commonly needed to invalidate external users cache.
     * 
     * @param username
     *            the username to search the {@link User} for delete
     */
    void deleteByUsername(String username);

}
