{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAF.Types
    (
    -- * Service Configuration
      waf

    -- * Errors
    , _WAFInvalidAccountException
    , _WAFReferencedItemException
    , _WAFInvalidOperationException
    , _WAFNonexistentItemException
    , _WAFInvalidParameterException
    , _WAFLimitsExceededException
    , _WAFStaleDataException
    , _WAFInternalErrorException
    , _WAFNonexistentContainerException
    , _WAFDisallowedNameException
    , _WAFNonEmptyEntityException

    -- * ChangeAction
    , ChangeAction (..)

    -- * ChangeTokenStatus
    , ChangeTokenStatus (..)

    -- * ComparisonOperator
    , ComparisonOperator (..)

    -- * IPSetDescriptorType
    , IPSetDescriptorType (..)

    -- * MatchFieldType
    , MatchFieldType (..)

    -- * PositionalConstraint
    , PositionalConstraint (..)

    -- * PredicateType
    , PredicateType (..)

    -- * TextTransformation
    , TextTransformation (..)

    -- * WafActionType
    , WafActionType (..)

    -- * ActivatedRule
    , ActivatedRule
    , activatedRule
    , arPriority
    , arRuleId
    , arAction

    -- * ByteMatchSet
    , ByteMatchSet
    , byteMatchSet
    , bmsName
    , bmsByteMatchSetId
    , bmsByteMatchTuples

    -- * ByteMatchSetSummary
    , ByteMatchSetSummary
    , byteMatchSetSummary
    , bmssByteMatchSetId
    , bmssName

    -- * ByteMatchSetUpdate
    , ByteMatchSetUpdate
    , byteMatchSetUpdate
    , bmsuAction
    , bmsuByteMatchTuple

    -- * ByteMatchTuple
    , ByteMatchTuple
    , byteMatchTuple
    , bmtFieldToMatch
    , bmtTargetString
    , bmtTextTransformation
    , bmtPositionalConstraint

    -- * FieldToMatch
    , FieldToMatch
    , fieldToMatch
    , ftmData
    , ftmType

    -- * HTTPHeader
    , HTTPHeader
    , hTTPHeader
    , httphValue
    , httphName

    -- * HTTPRequest
    , HTTPRequest
    , hTTPRequest
    , httprHTTPVersion
    , httprCountry
    , httprURI
    , httprHeaders
    , httprMethod
    , httprClientIP

    -- * IPSet
    , IPSet
    , ipSet
    , isName
    , isIPSetId
    , isIPSetDescriptors

    -- * IPSetDescriptor
    , IPSetDescriptor
    , ipSetDescriptor
    , isdType
    , isdValue

    -- * IPSetSummary
    , IPSetSummary
    , ipSetSummary
    , issIPSetId
    , issName

    -- * IPSetUpdate
    , IPSetUpdate
    , ipSetUpdate
    , isuAction
    , isuIPSetDescriptor

    -- * Predicate
    , Predicate
    , predicate
    , pNegated
    , pType
    , pDataId

    -- * Rule
    , Rule
    , rule
    , rMetricName
    , rName
    , rRuleId
    , rPredicates

    -- * RuleSummary
    , RuleSummary
    , ruleSummary
    , rsRuleId
    , rsName

    -- * RuleUpdate
    , RuleUpdate
    , ruleUpdate
    , ruAction
    , ruPredicate

    -- * SampledHTTPRequest
    , SampledHTTPRequest
    , sampledHTTPRequest
    , shttprAction
    , shttprTimestamp
    , shttprRequest
    , shttprWeight

    -- * SizeConstraint
    , SizeConstraint
    , sizeConstraint
    , scFieldToMatch
    , scTextTransformation
    , scComparisonOperator
    , scSize

    -- * SizeConstraintSet
    , SizeConstraintSet
    , sizeConstraintSet
    , scsName
    , scsSizeConstraintSetId
    , scsSizeConstraints

    -- * SizeConstraintSetSummary
    , SizeConstraintSetSummary
    , sizeConstraintSetSummary
    , scssSizeConstraintSetId
    , scssName

    -- * SizeConstraintSetUpdate
    , SizeConstraintSetUpdate
    , sizeConstraintSetUpdate
    , scsuAction
    , scsuSizeConstraint

    -- * SqlInjectionMatchSet
    , SqlInjectionMatchSet
    , sqlInjectionMatchSet
    , simsName
    , simsSqlInjectionMatchSetId
    , simsSqlInjectionMatchTuples

    -- * SqlInjectionMatchSetSummary
    , SqlInjectionMatchSetSummary
    , sqlInjectionMatchSetSummary
    , simssSqlInjectionMatchSetId
    , simssName

    -- * SqlInjectionMatchSetUpdate
    , SqlInjectionMatchSetUpdate
    , sqlInjectionMatchSetUpdate
    , simsuAction
    , simsuSqlInjectionMatchTuple

    -- * SqlInjectionMatchTuple
    , SqlInjectionMatchTuple
    , sqlInjectionMatchTuple
    , simtFieldToMatch
    , simtTextTransformation

    -- * TimeWindow
    , TimeWindow
    , timeWindow
    , twStartTime
    , twEndTime

    -- * WafAction
    , WafAction
    , wafAction
    , waType

    -- * WebACL
    , WebACL
    , webACL
    , waMetricName
    , waName
    , waWebACLId
    , waDefaultAction
    , waRules

    -- * WebACLSummary
    , WebACLSummary
    , webACLSummary
    , wasWebACLId
    , wasName

    -- * WebACLUpdate
    , WebACLUpdate
    , webACLUpdate
    , wauAction
    , wauActivatedRule

    -- * XSSMatchSet
    , XSSMatchSet
    , xssMatchSet
    , xmsName
    , xmsXSSMatchSetId
    , xmsXSSMatchTuples

    -- * XSSMatchSetSummary
    , XSSMatchSetSummary
    , xssMatchSetSummary
    , xmssXSSMatchSetId
    , xmssName

    -- * XSSMatchSetUpdate
    , XSSMatchSetUpdate
    , xssMatchSetUpdate
    , xmsuAction
    , xmsuXSSMatchTuple

    -- * XSSMatchTuple
    , XSSMatchTuple
    , xssMatchTuple
    , xmtFieldToMatch
    , xmtTextTransformation
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Network.AWS.WAF.Types.Product
import           Network.AWS.WAF.Types.Sum

-- | API version @2015-08-24@ of the Amazon WAF SDK configuration.
waf :: Service
waf =
    Service
    { _svcAbbrev = "WAF"
    , _svcSigner = v4
    , _svcPrefix = "waf"
    , _svcVersion = "2015-08-24"
    , _svcEndpoint = defaultEndpoint waf
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "WAF"
    , _svcRetry = retry
    }
  where
    retry =
        Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | The operation failed because you tried to create, update, or delete an object by using an invalid account identifier.
--
--
_WAFInvalidAccountException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInvalidAccountException =
    _ServiceError . hasCode "WAFInvalidAccountException"

-- | The operation failed because you tried to delete an object that is still in use. For example:
--
--
--     * You tried to delete a @ByteMatchSet@ that is still referenced by a @Rule@ .
--
--     * You tried to delete a @Rule@ that is still referenced by a @WebACL@ .
--
--
--
_WAFReferencedItemException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFReferencedItemException =
    _ServiceError . hasCode "WAFReferencedItemException"

-- | The operation failed because there was nothing to do. For example:
--
--
--     * You tried to remove a @Rule@ from a @WebACL@ , but the @Rule@ isn't in the specified @WebACL@ .
--
--     * You tried to remove an IP address from an @IPSet@ , but the IP address isn't in the specified @IPSet@ .
--
--     * You tried to remove a @ByteMatchTuple@ from a @ByteMatchSet@ , but the @ByteMatchTuple@ isn't in the specified @WebACL@ .
--
--     * You tried to add a @Rule@ to a @WebACL@ , but the @Rule@ already exists in the specified @WebACL@ .
--
--     * You tried to add an IP address to an @IPSet@ , but the IP address already exists in the specified @IPSet@ .
--
--     * You tried to add a @ByteMatchTuple@ to a @ByteMatchSet@ , but the @ByteMatchTuple@ already exists in the specified @WebACL@ .
--
--
--
_WAFInvalidOperationException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInvalidOperationException =
    _ServiceError . hasCode "WAFInvalidOperationException"

-- | The operation failed because the referenced object doesn't exist.
--
--
_WAFNonexistentItemException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFNonexistentItemException =
    _ServiceError . hasCode "WAFNonexistentItemException"

-- | The operation failed because AWS WAF didn't recognize a parameter in the request. For example:
--
--
--     * You specified an invalid parameter name.
--
--     * You specified an invalid value.
--
--     * You tried to update an object (@ByteMatchSet@ , @IPSet@ , @Rule@ , or @WebACL@ ) using an action other than @INSERT@ or @DELETE@ .
--
--     * You tried to create a @WebACL@ with a @DefaultAction@ @Type@ other than @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
--     * You tried to update a @WebACL@ with a @WafAction@ @Type@ other than @ALLOW@ , @BLOCK@ , or @COUNT@ .
--
--     * You tried to update a @ByteMatchSet@ with a @FieldToMatch@ @Type@ other than HEADER, QUERY_STRING, or URI.
--
--     * You tried to update a @ByteMatchSet@ with a @Field@ of @HEADER@ but no value for @Data@ .
--
--
--
_WAFInvalidParameterException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInvalidParameterException =
    _ServiceError . hasCode "WAFInvalidParameterException"

-- | The operation exceeds a resource limit, for example, the maximum number of @WebACL@ objects that you can create for an AWS account. For more information, see <http://docs.aws.amazon.com/waf/latest/developerguide/limits.html Limits> in the /AWS WAF Developer Guide/ .
--
--
_WAFLimitsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFLimitsExceededException =
    _ServiceError . hasCode "WAFLimitsExceededException"

-- | The operation failed because you tried to create, update, or delete an object by using a change token that has already been used.
--
--
_WAFStaleDataException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFStaleDataException = _ServiceError . hasCode "WAFStaleDataException"

-- | The operation failed because of a system problem, even though the request was valid. Retry your request.
--
--
_WAFInternalErrorException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFInternalErrorException =
    _ServiceError . hasCode "WAFInternalErrorException"

-- | The operation failed because you tried to add an object to or delete an object from another object that doesn't exist. For example:
--
--
--     * You tried to add a @Rule@ to or delete a @Rule@ from a @WebACL@ that doesn't exist.
--
--     * You tried to add a @ByteMatchSet@ to or delete a @ByteMatchSet@ from a @Rule@ that doesn't exist.
--
--     * You tried to add an IP address to or delete an IP address from an @IPSet@ that doesn't exist.
--
--     * You tried to add a @ByteMatchTuple@ to or delete a @ByteMatchTuple@ from a @ByteMatchSet@ that doesn't exist.
--
--
--
_WAFNonexistentContainerException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFNonexistentContainerException =
    _ServiceError . hasCode "WAFNonexistentContainerException"

-- | The name specified is invalid.
--
--
_WAFDisallowedNameException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFDisallowedNameException =
    _ServiceError . hasCode "WAFDisallowedNameException"

-- | The operation failed because you tried to delete an object that isn't empty. For example:
--
--
--     * You tried to delete a @WebACL@ that still contains one or more @Rule@ objects.
--
--     * You tried to delete a @Rule@ that still contains one or more @ByteMatchSet@ objects or other predicates.
--
--     * You tried to delete a @ByteMatchSet@ that contains one or more @ByteMatchTuple@ objects.
--
--     * You tried to delete an @IPSet@ that references one or more IP addresses.
--
--
--
_WAFNonEmptyEntityException :: AsError a => Getting (First ServiceError) a ServiceError
_WAFNonEmptyEntityException =
    _ServiceError . hasCode "WAFNonEmptyEntityException"
