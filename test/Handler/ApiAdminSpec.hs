{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.ApiAdminSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
    describe "Admin API" $ do
        it "returns dashboard data for admins" $ do
            adminEntity <- createAdmin "site-admin"
            Entity authorId _ <- createUser "member-author"
            Entity articleId _ <- createArticle authorId "Admin Visible Post" "admin-visible-post"
            _ <- createComment articleId "reader" "Admin sees this"
            _ <- createImage "admin.png" (Just "Admin image")
            authenticateAs adminEntity

            get ApiAdminDashboardR
            statusIs 200
            bodyContains "\"articleCount\":1"
            bodyContains "\"commentCount\":1"
            bodyContains "\"userCount\":2"
            bodyContains "\"imageCount\":1"
            bodyContains "\"title\":\"Admin Visible Post\""
            bodyContains "\"ident\":\"member-author\""

        it "deletes articles with dependent tags and comments" $ do
            adminEntity <- createAdmin "article-admin"
            Entity authorId _ <- createUser "article-owner"
            Entity articleId _ <- createArticleWithContent authorId "Delete Target" "body" "delete-target" False ["cleanup"]
            _ <- createComment articleId "reader" "delete comment"
            authenticateAs adminEntity

            requestWithCsrf $ do
                setMethod "POST"
                setUrl $ ApiAdminArticleDeleteR articleId

            statusIs 200

            deletedArticle <- getRecord articleId
            assertEq "deleted article flag" (isNothing deletedArticle) True
            remainingTags <- runDB $ selectList [TagArticle ==. articleId] []
            remainingComments <- runDB $ selectList [CommentArticle ==. articleId] []
            assertEq "remaining tags" (length remainingTags) 0
            assertEq "remaining comments" (length remainingComments) 0

        it "deletes users and their authored articles but blocks self-delete" $ do
            adminEntity@(Entity adminId _) <- createAdmin "user-admin"
            Entity memberId _ <- createUser "delete-member"
            Entity articleId _ <- createArticle memberId "Member Post" "member-post"
            _ <- createComment articleId "reader" "member comment"
            authenticateAs adminEntity

            requestWithCsrf $ do
                setMethod "POST"
                setUrl $ ApiAdminUserDeleteR memberId

            statusIs 200

            deletedUser <- getRecord memberId
            deletedArticle <- getRecord articleId
            assertEq "deleted user flag" (isNothing deletedUser) True
            assertEq "deleted article flag" (isNothing deletedArticle) True

            requestWithCsrf $ do
                setMethod "POST"
                setUrl $ ApiAdminUserDeleteR adminId

            statusIs 400
            bodyContains "cannot delete your own admin account"

        it "keeps theme purchase confirmation admin-only" $ do
            buyerEntity@(Entity buyerId _) <- createUser "theme-buyer"
            Entity authorId _ <- createUser "theme-seller"
            now <- liftIO getCurrentTime
            themeId <- runDB $ insert Theme
                { themeName = "Paid Theme"
                , themeSlug = "paid-theme"
                , themeDescription = Nothing
                , themeAuthor = Just authorId
                , themeParent = Nothing
                , themeBackgroundColor = "#ffffff"
                , themeSurfaceColor = "#ffffff"
                , themeTextColor = "#111111"
                , themeAccentColor = "#ffcc00"
                , themeHeadingFont = Nothing
                , themeBodyFont = Nothing
                , themeHeaderTemplate = Nothing
                , themeBodyTemplate = Nothing
                , themeFooterTemplate = Nothing
                , themeCustomCss = Nothing
                , themePriceCents = 1200
                , themeStatus = "published"
                , themeLicense = Just "marketplace-remix"
                , themeActive = True
                , themeCreatedAt = now
                , themeUpdatedAt = now
                }
            authenticateAs buyerEntity

            requestWithCsrf $ do
                setMethod "POST"
                setUrl $ ApiThemePurchaseR themeId

            statusIs 200

            requestWithCsrf $ do
                setMethod "POST"
                setUrl $ ApiThemePurchaseConfirmR themeId
                addPostParam "userId" (tshow $ fromSqlKey buyerId)
                addPostParam "status" "paid"

            statusIs 403

        it "marks membership orders paid and activates the membership" $ do
            adminEntity <- createAdmin "membership-admin"
            writerEntity@(Entity writerId _) <- createUser "membership-writer"
            memberEntity <- createUser "membership-reader"
            runDB $ update writerId
                [ UserPlan =. "writer-pro"
                , UserMembershipPriceCents =. 2400
                ]

            authenticateAs memberEntity
            requestWithCsrf $ do
                setMethod "POST"
                setUrl $ ApiUserMembershipR "membership-writer"
                addPostParam "mode" "request"

            statusIs 200

            membershipOrders <- runDB $ selectList [MembershipOrderCreator ==. writerId] []
            case membershipOrders of
                [Entity orderId order] -> do
                    assertEq "membership order status" (membershipOrderStatus order) "pending"

                    authenticateAs adminEntity
                    requestWithCsrf $ do
                        setMethod "POST"
                        setUrl $ ApiAdminMembershipOrderUpdateR orderId
                        addPostParam "status" "paid"

                    statusIs 200

                    memberships <- runDB $ selectList [MembershipCreator ==. writerId] []
                    case memberships of
                        [Entity _ membership] -> do
                            assertEq "membership activated" (membershipStatus membership) "active"
                            assertEq "membership member linked" (membershipMember membership /= writerId) True
                        _ -> liftIO $ expectationFailure "expected one membership after payment approval"
                _ -> liftIO $ expectationFailure "expected one membership order after request"

        it "requires a note when admins fail membership orders" $ do
            adminEntity <- createAdmin "membership-fail-admin"
            Entity writerId _ <- createUser "membership-fail-writer"
            memberEntity <- createUser "membership-fail-reader"
            runDB $ update writerId
                [ UserPlan =. "writer-pro"
                , UserMembershipPriceCents =. 1900
                ]

            authenticateAs memberEntity
            requestWithCsrf $ do
                setMethod "POST"
                setUrl $ ApiUserMembershipR "membership-fail-writer"
                addPostParam "mode" "request"

            statusIs 200

            membershipOrders <- runDB $ selectList [MembershipOrderCreator ==. writerId] []
            case membershipOrders of
                [Entity orderId _] -> do
                    authenticateAs adminEntity
                    requestWithCsrf $ do
                        setMethod "POST"
                        setUrl $ ApiAdminMembershipOrderUpdateR orderId
                        addPostParam "status" "failed"

                    statusIs 400
                    bodyContains "note is required"
                _ -> liftIO $ expectationFailure "expected one membership order after request"

        it "creates a renewal order when auto-renew memberships expire" $ do
            writerEntity@(Entity writerId _) <- createUser "renewal-writer"
            memberEntity@(Entity memberId _) <- createUser "renewal-reader"
            now <- liftIO getCurrentTime
            _ <- runDB $ insert Membership
                { membershipCreator = writerId
                , membershipMember = memberId
                , membershipPriceCents = 3600
                , membershipStatus = "active"
                , membershipAutoRenew = True
                , membershipStartedAt = Just now
                , membershipExpiresAt = Just now
                , membershipCreatedAt = now
                , membershipUpdatedAt = now
                }

            authenticateAs memberEntity
            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiMeMembershipRefreshR
            statusIs 200

            renewalOrders <- runDB $ selectList [MembershipOrderMember ==. memberId, MembershipOrderStatus ==. "pending"] []
            case renewalOrders of
                [Entity _ order] ->
                    assertEq "renewal provider" (membershipOrderProvider order) (Just "membership-renewal")
                _ -> liftIO $ expectationFailure "expected one pending renewal order"

            memberships <- runDB $ selectList [MembershipCreator ==. writerId, MembershipMember ==. memberId] []
            case memberships of
                [Entity _ membership] ->
                    assertEq "membership moved to pending" (membershipStatus membership) "pending"
                _ -> liftIO $ expectationFailure "expected one membership after renewal check"

        it "lets members cancel and retry pending renewal orders" $ do
            Entity writerId _ <- createUser "renewal-control-writer"
            memberEntity@(Entity memberId _) <- createUser "renewal-control-reader"
            now <- liftIO getCurrentTime
            membershipId <- runDB $ insert Membership
                { membershipCreator = writerId
                , membershipMember = memberId
                , membershipPriceCents = 4200
                , membershipStatus = "active"
                , membershipAutoRenew = True
                , membershipStartedAt = Just now
                , membershipExpiresAt = Just now
                , membershipCreatedAt = now
                , membershipUpdatedAt = now
                }

            authenticateAs memberEntity
            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiMeMembershipRefreshR
            statusIs 200

            renewalOrders <- runDB $ selectList [MembershipOrderMembership ==. membershipId, MembershipOrderStatus ==. "pending"] []
            case renewalOrders of
                [Entity orderId _] -> do
                    requestWithCsrf $ do
                        setMethod "POST"
                        setUrl $ ApiMeMembershipOrderUpdateR orderId
                        addPostParam "mode" "cancel"

                    statusIs 200

                    mCancelledOrder <- getRecord orderId
                    mCancelledMembership <- getRecord membershipId
                    case (mCancelledOrder, mCancelledMembership) of
                        (Just cancelledOrder, Just cancelledMembership) -> do
                            assertEq "renewal cancelled" (membershipOrderStatus cancelledOrder) "cancelled"
                            assertEq "membership expired after cancellation" (membershipStatus cancelledMembership) "expired"
                            assertEq "auto renew disabled after cancellation" (membershipAutoRenew cancelledMembership) False
                        _ -> liftIO $ expectationFailure "expected cancelled order and membership to exist"

                    requestWithCsrf $ do
                        setMethod "POST"
                        setUrl $ ApiMeMembershipOrderUpdateR orderId
                        addPostParam "mode" "retry"

                    statusIs 200

                    mRetriedOrder <- getRecord orderId
                    mRetriedMembership <- getRecord membershipId
                    case (mRetriedOrder, mRetriedMembership) of
                        (Just retriedOrder, Just retriedMembership) -> do
                            assertEq "renewal retried" (membershipOrderStatus retriedOrder) "pending"
                            assertEq "membership moved back to pending after retry" (membershipStatus retriedMembership) "pending"
                        _ -> liftIO $ expectationFailure "expected retried order and membership to exist"
                _ -> liftIO $ expectationFailure "expected one pending renewal order before cancel/retry"

        it "rejects unsafe theme templates" $ do
            userEntity@(Entity userId _) <- createUser "theme-template-author"
            now <- liftIO getCurrentTime
            runDB $ update userId [UserPlan =. "designer-pro", UserPlanExpiresAt =. Just now]
            authenticateAs userEntity

            requestWithCsrf $ do
                setMethod "POST"
                setUrl ApiThemeCreateR
                addPostParam "name" "Unsafe Theme"
                addPostParam "slug" "unsafe-theme"
                addPostParam "description" "theme"
                addPostParam "backgroundColor" "#ffffff"
                addPostParam "surfaceColor" "#f5f5f5"
                addPostParam "textColor" "#111111"
                addPostParam "accentColor" "#ffcc00"
                addPostParam "headingFont" "Space Grotesk"
                addPostParam "bodyFont" "Space Grotesk"
                addPostParam "headerTemplate" "<script>alert(1)</script>"
                addPostParam "bodyTemplate" "<article>{{post.content}}</article>"
                addPostParam "footerTemplate" "<footer>safe</footer>"
                addPostParam "customCss" ""
                addPostParam "priceCents" "0"

            statusIs 400
            bodyContains "safe structural HTML"
