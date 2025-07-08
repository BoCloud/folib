package com.folib.converters.users;

import com.folib.users.dto.UserDto;
import com.folib.forms.users.UserForm;

import org.springframework.core.convert.converter.Converter;

/**
 * @author Veadan
 * @author veadan
 */
public enum UserFormToUserDtoConverter
        implements Converter<UserForm, UserDto>
{

    INSTANCE;

    @Override
    public UserDto convert(UserForm userForm)
    {
        UserDto user = new UserDto();
        user.setUsername(userForm.getUsername());
        user.setPassword(userForm.getPassword());
        user.setOriginalPassword(userForm.getOriginalPassword());
        user.setEnabled(userForm.isEnabled());
        user.setRoleNames(userForm.getRoles());
        user.setSecurityTokenKey(userForm.getSecurityTokenKey());
        user.setEmail(userForm.getEmail());

        return user;
    }
}
