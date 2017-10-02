using IdentityServer4;
using IdentityServer4.Models;
using System.Collections.Generic;
using System.Security.Claims;
using IdentityServer4.Test;


namespace FPPKGIdentityServer
{
    public class Config
    {
        // scopes define the resources in your system
        public static IEnumerable<IdentityResource> GetIdentityResources()
        {
            var customRole = new IdentityResource(
                name: "role",
                displayName: "Role",
                claimTypes: new[] { "role" });

            return new List<IdentityResource>
            {
                new IdentityResources.OpenId(),
                new IdentityResources.Profile(),
                customRole
            };
        }

        public static IEnumerable<ApiResource> GetApiResources()
        {
            return new List<ApiResource>
            {
              new ApiResource
                {
                    // secret for using introspection endpoint
                    ApiSecrets =
                    {
                        new Secret("secret".Sha256())
                    },

                    // include the following using claims in access token (in addition to subject id)
                    UserClaims = new List<string> { "role" },
                    Scopes = new List<Scope> {
                      new Scope("buildagent") {
                        Name = "buildagent",
                        DisplayName = "Build packages",
                        Description = "Test and build packages",
                      }
                    }

                }
            };
        }

        // clients want to access resources (aka scopes)
        public static IEnumerable<Client> GetClients()
        {
            // client credentials client
            return new List<Client>
            {
                new Client
                {
                    ClientId = "FPPKGWebClient",
                    ClientName = "FPPKG Web Client",
                    AllowedGrantTypes = GrantTypes.Implicit,

                    ClientSecrets =
                    {
                        new Secret("secret".Sha256())
                    },
                    AllowedScopes = {
                        "role",
                        "buildagent",
                        IdentityServerConstants.StandardScopes.OpenId,
                        IdentityServerConstants.StandardScopes.Profile,
                    },
                    RedirectUris = { "http://localhost:4200" },
                    PostLogoutRedirectUris = { "http://localhost:4200/unauthorized" },
                    AllowedCorsOrigins = { "http://localhost:4200" },
                    AllowAccessTokensViaBrowser = true
                }
            };
        }

       public static List<TestUser> GetUsers()
        {
            return new List<TestUser>
            {
                new TestUser
                {
                    SubjectId = "1",
                    Username = "alice",
                    Password = "password",

                    Claims = new List<Claim>
                    {
                        new Claim("name", "Alice"),
                        new Claim("role", "user")
                    }
                },
                new TestUser
                {
                    SubjectId = "2",
                    Username = "joost",
                    Password = "jachtluipaard",

                    Claims = new List<Claim>
                    {
                        new Claim("name", "Joost van der Sluis"),
                        new Claim("role", "admin")
                    }
                }
            };
        }
    }
}