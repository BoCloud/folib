package com.folib.users.userdetails;

import java.util.function.Function;

import com.folib.domain.User;
import org.springframework.security.core.userdetails.UserDetails;

public interface FolibUserToUserDetails extends Function<User, UserDetails>
{

}
